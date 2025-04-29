{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The Ollama plugin for GHC
module GHC.Plugin.OllamaHoles (plugin) where

import Control.Monad (unless, when)
import Data.Char (isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Plugins hiding ((<>))
import GHC.Tc.Types
import GHC.Tc.Types.Constraint (Hole (..), ctLocEnv, ctLocSpan)
import GHC.Tc.Utils.Monad (getGblEnv, newTcRef)

import GHC.Plugin.OllamaHoles.Backend

import GHC.Plugin.OllamaHoles.Backend.Ollama (ollamaBackend)
import GHC.Plugin.OllamaHoles.Backend.OpenAI (openAIBackend)
import GHC.Plugin.OllamaHoles.Backend.Gemini (geminiBackend)

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

getBackend :: Text -> Backend
getBackend "ollama" = ollamaBackend
getBackend "openai" = openAIBackend
getBackend "gemini" = geminiBackend
getBackend b = error $ "unknown backend: " <> T.unpack b

pluginName :: Text
pluginName = "Ollama Plugin"

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
    fitPlugin opts hole fits = do
        let Flags{..} = parseFlags opts
        dflags <- getDynFlags
        gbl_env <- getGblEnv
        let mod_name = moduleNameString $ moduleName $ tcg_mod gbl_env
            imports = tcg_imports gbl_env
        let backend = getBackend backend_name
        liftIO $ do
            available_models <- listModels backend
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
                    when debug $ T.putStrLn $ "--- " <> pluginName <> ": Hole Found ---"
                    let mn = "Module: " <> mod_name
                    let lc = "Location: " <> showSDoc dflags (ppr $ ctLocSpan . hole_loc <$> th_hole hole)
                    let im = "Imports: " <> showSDoc dflags (ppr $ moduleEnvKeys $ imp_mods imports)

                    case th_hole hole of
                        Just h -> do
                            let lcl_env = ctLocEnv (hole_loc h)
                            let hv = "Hole variable: _" <> occNameString (occName $ hole_occ h)
                            let ht = "Hole type: " <> showSDoc dflags (ppr $ hole_ty h)
                            let rc = "Relevant constraints: " <> showSDoc dflags (ppr $ th_relevant_cts hole)
                            let le = "Local environment (bindings): " <> showSDoc dflags (ppr $ tcl_rdr lcl_env)
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
                            res <- generateFits backend prompt' model_name
                            case res of
                                Right rsp -> do
                                    let lns = (preProcess . T.lines) rsp
                                    when debug $ do
                                        T.putStrLn $ "--- " <> pluginName <> ": Prompt ---\n" <> prompt'
                                        T.putStrLn $ "--- " <> pluginName <> ": Response ---\n" <> rsp
                                    let fits' = map (RawHoleFit . text . T.unpack) lns
                                    -- Return the generated fits
                                    return fits'
                                Left err -> do
                                    when debug $
                                        putStrLn $
                                            T.unpack pluginName <> " failed to generate a response.\n" <> err
                                    -- Return the original fits without modification
                                    return fits
                        Nothing -> return fits

-- | Preprocess the response to remove empty lines, lines with only spaces, and code blocks
preProcess :: [Text] -> [Text]
preProcess [] = []
-- \| Remove lines between <think> and </think> tags from e.g. deepseek
preProcess (ln : lns)
    | T.isPrefixOf "<think>" ln =
        preProcess (tail $ dropWhile (not . T.isPrefixOf "</think>") lns)
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
    }

-- | Default flags for the plugin
defaultFlags :: Flags
defaultFlags =
    Flags
        { model_name = "gemma3:27b-it-qat"
        , backend_name = "ollama"
        , num_expr = 5
        , debug = False
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
