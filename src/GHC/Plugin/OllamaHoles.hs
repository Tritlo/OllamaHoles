{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GHC.Plugin.OllamaHoles (plugin) where

import GHC.Plugins hiding ((<>))
import GHC.Tc.Types
import GHC.Tc.Utils.Monad (newTcRef, getGblEnv)
import GHC.Tc.Types.Constraint (Hole(..), ctLocEnv, ctLocSpan)
import Ollama (GenerateOps(..), defaultGenerateOps, generate, response_)
import qualified Data.Text as T
import Control.Monad (when)


genOps :: GenerateOps
genOps = defaultGenerateOps
          { modelName = ""
          , prompt = ""
          }

promptTemplate :: String
promptTemplate =
    "You are a typed-hole plugin within GHC, the Glasgow Haskell Compiler.\n" <>
    "You are given a hole in a Haskell program, and you need to fill it in.\n" <>
    "The hole is represented by the following information:\n" <>
    "{module}\n{location}\n{imports}\n{hole_var}\n{hole_type}\n{relevant_constraints}\n{local_env}\n{global_env}\n{candidate_fits}\n\n" <>
    "Provide one or more Haskell expressions that could fill this hole.\n"<> 
    "This means coming up with an expression of the correct type that satisfies the constraints.\n" <>
    "Pay special attention to the type of the hole, specifically whether it is a function.\n" <>
    "Make sure you synthesize an expression that matches the type of the hole.\n" <>
    "Output ONLY the raw Haskell expression(s), one per line.\n" <>
    --"Include an explanation right after the expression, preceeded by a --.\n" <>
    "Do not include explanations, introductions, or any surrounding text.\n"<>
    "Feel free to any other functions from the list of imports to generate more complicated expressions.\n" <>
    "Output a maximum of {numexpr} expresssions.\n" 

plugin :: Plugin
plugin = defaultPlugin
    { holeFitPlugin = \opts -> Just $ HoleFitPluginR
        { hfPluginInit = newTcRef ()
        , hfPluginStop = \_ -> return ()
        , hfPluginRun = const HoleFitPlugin
            { candPlugin = \_ c -> return c -- Don't filter candidates
            , fitPlugin = \hole fits -> do
                let Flags{..} = parseFlags opts
                dflags <- getDynFlags
                gbl_env <- getGblEnv
                let mod_name = moduleNameString $ moduleName $ tcg_mod gbl_env
                    imports = tcg_imports gbl_env
                when debug $
                  liftIO $ putStrLn "--- Ollama Plugin: Hole Found ---"
                let mn = "Module: " ++ mod_name
                let lc = "Location: " ++ showSDoc dflags (ppr $ ctLocSpan . hole_loc <$> th_hole hole)
                let im = "Imports: " ++ showSDoc dflags (ppr $ moduleEnvKeys $  imp_mods imports)

                case th_hole hole of
                  Just h -> do
                    let lcl_env = ctLocEnv (hole_loc h)
                    let hv = "Hole variable: _" ++ occNameString (occName $ hole_occ h)
                    let ht = "Hole type: " ++ showSDoc dflags (ppr $ hole_ty h)
                    let rc = "Relevant constraints: " ++ showSDoc dflags (ppr $ th_relevant_cts hole)
                    let le = "Local environment (bindings): " ++ showSDoc dflags (ppr $ tcl_rdr lcl_env)
                    let ge = "Global environment (bindings): " ++ showSDoc dflags (ppr $ tcg_binds gbl_env)
                    let cf = "Candidate fits: " ++ showSDoc dflags (ppr fits)
                    let prompt' = replacePlaceholders promptTemplate [
                                       ("{module}", mn),
                                       ("{location}", lc),
                                       ("{imports}", im),
                                       ("{hole_var}", hv),
                                       ("{hole_type}", ht),
                                       ("{relevant_constraints}", rc),
                                       ("{local_env}", le),
                                       ("{global_env}", ge),
                                       ("{candidate_fits}", cf),
                                       ("{numexpr}", show num_expr)
                                     ]
                    res <- liftIO $ generate genOps { prompt =  T.pack prompt', modelName = T.pack model_name }
                    case res of
                      Right rsp -> do
                        let lns = T.lines $ response_ rsp
                        when debug $ do
                          liftIO $ putStrLn $ "--- Ollama Plugin: Prompt ---\n" <> prompt'
                          liftIO $ putStrLn $ "--- Ollama Plugin: Response ---\n" <> T.unpack (response_ rsp)
                        let fits' = map (RawHoleFit . text . T.unpack) lns
                        -- Return the generated fits
                        return fits'
                      Left err -> do
                        when debug $
                          liftIO $ putStrLn $ "Ollama plugin failed to generate a response.\n" <> err
                        -- Return the original fits without modification
                        return fits

                  Nothing -> return fits
            }
        }
    }

data Flags = Flags
    { model_name :: String
    , num_expr :: Int
    , debug :: Bool
    }

defaultFlags :: Flags
defaultFlags = Flags
    { model_name = "gemma3:27b"
    , num_expr = 5
    , debug = False
    }

parseFlags :: [CommandLineOption] -> Flags
parseFlags = parseFlags' defaultFlags
  where
    parseFlags' :: Flags -> [CommandLineOption] -> Flags
    parseFlags' flags [] = flags
    parseFlags' flags (opt:opts) | T.isPrefixOf "model=" (T.pack opt) =
        let model_name = T.unpack $ T.drop (T.length "model=") (T.pack opt)
        in parseFlags' flags { model_name = model_name } opts
    parseFlags' flags (opt:opts) | T.isPrefixOf "debug=" (T.pack opt) =
        let debug = T.unpack $ T.drop (T.length "debug=") (T.pack opt)
        in parseFlags' flags { debug = read debug } opts
    parseFlags' flags (opt:opts) | T.isPrefixOf "n=" (T.pack opt) =
        let num_expr = T.unpack $ T.drop (T.length "n=") (T.pack opt)
        in parseFlags' flags { num_expr = read num_expr } opts
    parseFlags' flags _ = flags
   


-- Helper function to replace placeholders in a template string
replacePlaceholders :: String -> [(String, String)] -> String
replacePlaceholders = foldl replacePlaceholder
  where
    replacePlaceholder :: String -> (String, String) -> String
    replacePlaceholder str (placeholder, value) = T.unpack $ T.replace (T.pack placeholder) (T.pack value) (T.pack str)
