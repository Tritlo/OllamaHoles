{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The Ollama plugin for GHC
module GHC.Plugin.OllamaHoles (plugin) where

import Control.Monad (filterM, unless, when)
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Plugins hiding ((<>))
import GHC.Tc.Types
import GHC.Tc.Types.Constraint (Hole (..))
import GHC.Tc.Utils.Monad (getGblEnv, newTcRef)

import GHC.Plugin.OllamaHoles.Backend
import GHC.Plugin.OllamaHoles.Backend.Gemini (geminiBackend)
import GHC.Plugin.OllamaHoles.Backend.Ollama (ollamaBackend)
import GHC.Plugin.OllamaHoles.Backend.OpenAI (openAICompatibleBackend)

import Control.Monad.Catch (handleAll)
import GHC (GhcPs, LHsExpr)
import GHC.Data.StringBuffer qualified as GHC (stringToStringBuffer)
import GHC.Driver.Config.Parser qualified as GHC (initParserOpts)
import GHC.Parser qualified as GHC (parseExpression)
import GHC.Parser.Lexer qualified as GHC (ParseResult (..), getPsErrorMessages, initParserState, unP)
import GHC.Parser.PostProcess qualified as GHC (runPV, unECP)
import GHC.Rename.Expr qualified as GHC (rnLExpr)
import GHC.Tc.Errors.Hole qualified as GHC (tcCheckHoleFit, withoutUnification)
import GHC.Tc.Gen.App qualified as GHC (tcInferSigma)
import GHC.Tc.Utils.TcType qualified as GHC (tyCoFVsOfType)
import GHC.Types.SrcLoc qualified as GHC (mkRealSrcLoc)
#if __GLASGOW_HASKELL__ >= 908
import GHC.Tc.Types.Constraint (CtLocEnv(..))
#endif

#if __GLASGOW_HASKELL__ >= 912
import GHC.Tc.Types.CtLoc (ctLocEnv, ctLocSpan)
import qualified Data.Map as Map
#else
import GHC.Tc.Types.Constraint (ctLocEnv, ctLocSpan)
#endif

-- | Prompt used to prompt the LLM
promptTemplate :: Text
promptTemplate =
    "You are a typed-hole plugin within GHC, the Glasgow Haskell Compiler.\n"
        <> "You are given a hole in a Haskell program, and you need to fill it in.\n"
        <> "The hole is represented by the following information:\n"
        <> "{module}\n{location}\n{imports}\n{hole_var}\n{hole_type}\n{relevant_constraints}\n{local_env}\n{global_env}\n{candidate_fits}\n\n"
        <> "Provide one or more Haskell expressions that could fill this hole.\n"
        <> "This means coming up with an expression of the correct type that satisfies the constraints.\n"
        <> "Pay special attention to the type of the hole, specifically whether it is a function.\n"
        <> "Make sure you synthesize an expression that matches the type of the hole.\n"
        <> "Output ONLY the raw Haskell expression(s), one per line.\n"
        <> "Do not include explanations, introductions, or any surrounding text.\n"
        <> "Feel free to include any other functions from the list of imports to generate more complicated expressions.\n"
        <> "Output a maximum of {numexpr} expresssions.\n"

-- | Determine which backend to use
getBackend :: Flags -> Backend
getBackend Flags{backend_name = "ollama"} = ollamaBackend
getBackend Flags{backend_name = "gemini"} = geminiBackend
getBackend Flags{backend_name = "openai", ..} = openAICompatibleBackend openai_base_url openai_key_name
getBackend Flags{..} = error $ "unknown backend: " <> T.unpack backend_name

-- | Ollama plugin for GHC
plugin :: Plugin
plugin =
    defaultPlugin
        { holeFitPlugin = \opts ->
            Just $
                HoleFitPluginR
                    { hfPluginInit = newTcRef ()
                    , hfPluginStop = \_ -> return ()
                    , hfPluginRun =
                        const
                            HoleFitPlugin
                                { candPlugin = \_ c -> return c -- Don't filter candidates
                                , fitPlugin = fitPlugin opts
                                }
                    }
        }
  where
    pluginName = "Ollama Plugin"
    fitPlugin opts hole fits = do
        let flags@Flags{..} = parseFlags opts
        dflags <- getDynFlags
        gbl_env <- getGblEnv
        let mod_name = moduleNameString $ moduleName $ tcg_mod gbl_env
            imports = tcg_imports gbl_env
        let backend = getBackend flags
        available_models <- liftIO $ listModels backend
        case available_models of
            Nothing ->
                error $
                    "--- " <> T.unpack pluginName <> ": No models available, check your configuration ---"
            Just models -> do
                unless (model_name `elem` models) $
                    error $
                        "--- "
                            <> T.unpack pluginName
                            <> ": Model "
                            <> T.unpack model_name
                            <> " not found. "
                            <> ( if backend_name == "ollama"
                                    then "Use `ollama pull` to download the model, or "
                                    else ""
                               )
                            <> "specify another model using "
                            <> "`-fplugin-opt=GHC.Plugin.OllamaHoles:model=<model_name>` ---"
                            <> "--- Availble models: "
                            <> T.unpack (T.unlines models)
                            <> " ---"
                liftIO $ when debug $ T.putStrLn $ "--- " <> pluginName <> ": Hole Found ---"
                let mn = "Module: " <> mod_name
                let lc = "Location: " <> showSDoc dflags (ppr $ ctLocSpan . hole_loc <$> th_hole hole)
#if __GLASGOW_HASKELL__ >= 912
                let im = "Imports: " <> showSDoc dflags (ppr $ Map.keys $ imp_mods imports)
#else
                let im = "Imports: " <> showSDoc dflags (ppr $ moduleEnvKeys $ imp_mods imports)
#endif

                case th_hole hole of
                    Just h -> do
                        let lcl_env = ctLocEnv (hole_loc h)
                        let hv = "Hole variable: _" <> occNameString (occName $ hole_occ h)
                        let ht = "Hole type: " <> showSDoc dflags (ppr $ hole_ty h)
                        let rc = "Relevant constraints: " <> showSDoc dflags (ppr $ th_relevant_cts hole)

#if __GLASGOW_HASKELL__ >= 908
                        let le = "Local environment (bindings): " <> showSDoc dflags (ppr $ ctl_rdr lcl_env)
#else
                        let le = "Local environment (bindings): " <> showSDoc dflags (ppr $ tcl_rdr lcl_env)
#endif

                        let ge = "Global environment (bindings): " <> showSDoc dflags (ppr $ tcg_binds gbl_env)
                        let cf = "Candidate fits: " <> showSDoc dflags (ppr fits)
                        let prompt' =
                                replacePlaceholders
                                    promptTemplate
                                    [ ("{module}", mn)
                                    , ("{location}", lc)
                                    , ("{imports}", im)
                                    , ("{hole_var}", hv)
                                    , ("{hole_type}", ht)
                                    , ("{relevant_constraints}", rc)
                                    , ("{local_env}", le)
                                    , ("{global_env}", ge)
                                    , ("{candidate_fits}", cf)
                                    , ("{numexpr}", show num_expr)
                                    ]
                        res <- liftIO $ generateFits backend prompt' model_name
                        case res of
                            Right rsp -> do
                                let lns = (preProcess . T.lines) rsp
                                verified <- filterM (verifyHoleFit debug hole) lns
                                liftIO $ when debug $ do
                                    T.putStrLn $ "--- " <> pluginName <> ": Prompt ---\n" <> prompt'
                                    T.putStrLn $ "--- " <> pluginName <> ": Response ---\n" <> rsp
                                let fits' = map (RawHoleFit . text . T.unpack) verified
                                -- Return the generated fits
                                return fits'
                            Left err -> do
                                liftIO $
                                    when debug $
                                        T.putStrLn $
                                            pluginName <> " failed to generate a response.\n" <> T.pack err
                                -- Return the original fits without modification
                                return fits
                    Nothing -> return fits

-- | Check that the hole fit matches the type of the hole
verifyHoleFit :: Bool -> TypedHole -> Text -> TcM Bool
verifyHoleFit debug hole fit | Just h <- th_hole hole = do
    -- Instaniate a new IORef session with the current HscEnv.
    -- First we try parsing the suggest hole-fits
    dflags <- getDynFlags
    let parsed =
            GHC.unP (GHC.parseExpression >>= \p -> GHC.runPV $ GHC.unECP p) $
                GHC.initParserState
                    (GHC.initParserOpts dflags)
                    (GHC.stringToStringBuffer (T.unpack fit))
                    (GHC.mkRealSrcLoc (mkFastString "<hole-fit-validation>") 1 1)
    case parsed of
        GHC.PFailed st -> do
            when debug $
                liftIO $
                    putStrLn $
                        showPprUnsafe $
                            GHC.getPsErrorMessages st
            return False
        GHC.POk _ (p_e :: LHsExpr GhcPs) -> handleAll falseOnErr $ do
            -- If parsing was successful, we try renaming the expression
            (rn_e, free_vars) <- GHC.rnLExpr p_e
            when debug $
                liftIO $
                    putStrLn $
                        showPprUnsafe free_vars
            -- Finally, we infer the type of the expression
            expr_ty <- GHC.tcInferSigma False rn_e
            -- And check whether that is indeed a valid hole fit
            (does_fit, wrapper) <-
                GHC.withoutUnification (GHC.tyCoFVsOfType $ hole_ty h) $
                    GHC.tcCheckHoleFit hole (hole_ty h) expr_ty
            when debug $
                liftIO $
                    putStrLn $
                        showPprUnsafe wrapper
            return does_fit
  where
    falseOnErr e = liftIO $ do
        when debug $ print e
        return False
verifyHoleFit _ _ _ = return False

-- | Preprocess the response to remove empty lines, lines with only spaces, and code blocks
preProcess :: [Text] -> [Text]
preProcess [] = []
-- \| Remove lines between <think> and </think> tags from e.g. deepseek
preProcess (ln : lns)
    | T.isPrefixOf "<think>" ln =
        preProcess (drop 1 $ dropWhile (not . T.isPrefixOf "</think>") lns)
preProcess (ln : lns) | should_drop = preProcess lns
  where
    should_drop :: Bool
    should_drop =
        T.null ln
            || T.all isSpace ln
            || T.isPrefixOf "```" ln
preProcess (ln : lns) = transform ln : preProcess lns
  where
    transform :: Text -> Text
    transform = T.strip

-- | Command line options for the plugin
data Flags = Flags
    { model_name :: Text
    , backend_name :: Text
    , num_expr :: Int
    , debug :: Bool
    , openai_base_url :: Text
    , openai_key_name :: Text
    }

-- | Default flags for the plugin
defaultFlags :: Flags
defaultFlags =
    Flags
        { model_name = "gemma3:27b-it-qat"
        , backend_name = "ollama"
        , num_expr = 5
        , debug = False
        , openai_base_url = "https://api.openai.com"
        , openai_key_name = "OPENAI_API_KEY"
        }

-- | Parse command line options
parseFlags :: [CommandLineOption] -> Flags
parseFlags = parseFlags' defaultFlags
  where
    parseFlags' :: Flags -> [CommandLineOption] -> Flags
    parseFlags' flags [] = flags
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "model=" (T.pack opt) =
            let model_name = T.drop (T.length "model=") (T.pack opt)
             in parseFlags' flags{model_name = model_name} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "backend=" (T.pack opt) =
            let backend_name = T.drop (T.length "backend=") (T.pack opt)
             in parseFlags' flags{backend_name = backend_name} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "openai_base_url=" (T.pack opt) =
            let openai_base_url = T.drop (T.length "openai_base_url=") (T.pack opt)
             in parseFlags' flags{openai_base_url = openai_base_url} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "openai_key_name=" (T.pack opt) =
            let openai_key_name = T.drop (T.length "openai_key_name=") (T.pack opt)
             in parseFlags' flags{openai_key_name = openai_key_name} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "debug=" (T.pack opt) =
            let debug = T.unpack $ T.drop (T.length "debug=") (T.pack opt)
             in parseFlags' flags{debug = read debug} opts
    parseFlags' flags (opt : opts)
        | T.isPrefixOf "n=" (T.pack opt) =
            let num_expr = T.unpack $ T.drop (T.length "n=") (T.pack opt)
             in parseFlags' flags{num_expr = read num_expr} opts
    parseFlags' flags _ = flags

-- | Helper function to replace placeholders in a template string
replacePlaceholders :: Text -> [(Text, String)] -> Text
replacePlaceholders = foldl replacePlaceholder
  where
    replacePlaceholder :: Text -> (Text, String) -> Text
    replacePlaceholder str (placeholder, value) = T.replace placeholder (T.pack value) str
