module GHC.Plugin.OllamaHoles (plugin) where

import GHC.Plugins
import GHC.Tc.Types
import GHC.Tc.Utils.Monad (newTcRef, getGblEnv)
import GHC.Tc.Types.Constraint (Hole(..), ctLocEnv, ctLocSpan)
import GHC.Tc.Utils.Env () -- Import the module, but not specific names for fields
import Ollama (GenerateOps(..), defaultGenerateOps, generate, response_)
import qualified Data.Text.IO as T
import Control.Monad (void)


genOps :: GenerateOps
genOps = defaultGenerateOps
          { modelName = "gemma3" -- TODO: Make this configurable
          , prompt = ""
          , stream = Just (T.putStr . Ollama.response_, pure ())
          }



plugin :: Plugin
plugin = defaultPlugin
    { holeFitPlugin = \_ -> Just $ HoleFitPluginR
        { hfPluginInit = newTcRef ()
        , hfPluginStop = \_ -> return ()
        , hfPluginRun = const HoleFitPlugin
            { candPlugin = \_ c -> return c -- Don't filter candidates
            , fitPlugin = \hole fits -> do
                dflags <- getDynFlags
                gbl_env <- getGblEnv
                let mod_name = moduleNameString $ moduleName $ tcg_mod gbl_env
                    imports = tcg_imports gbl_env

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
                    let prompt =
                         "You are a typed-hole plugin within GHC, the Glasgow Haskell Compiler.\n" ++
                         "You are given a hole in a Haskell program, and you need to fill it in.\n" ++
                         "The hole is represented by the following information:\n" ++
                          mn ++ "\n" ++ lc ++ "\n" ++ im ++ "\n" ++ hv ++ "\n" ++ ht ++ "\n" ++ rc ++ "\n" ++ le ++ "\n" ++ ge ++ "\n" ++ cf
                    let prompt' = prompt ++ "\n\n" ++ "Please provide a few candidate Haskell expression to fill in the hole:\n"
                    liftIO $ putStrLn $ "--- Ollama Prompt ---\n" ++ prompt' ++ "\n---------------------"
                    liftIO $ void $ generate genOps { prompt = prompt' }

                  Nothing -> liftIO $ putStrLn "Hole details not available (e.g., out of scope variable)."

                liftIO $ putStrLn "--- Ollama Plugin: End Hole ---"
                -- Return the original fits without modification
                return fits
            }
        }
    }
module GHC.Plugin.OllamaHoles (plugin) where

import GHC.Plugins
import GHC.Tc.Types
import GHC.Tc.Utils.Monad (newTcRef, getGblEnv)
import GHC.Tc.Types.Constraint (Hole(..), ctLocEnv, ctLocSpan)
import GHC.Tc.Utils.Env () -- Import the module, but not specific names for fields
import Ollama (GenerateOps(..), defaultGenerateOps, generate, response_)
import qualified Data.Text.IO as T
import Control.Monad (void)


genOps :: GenerateOps
genOps = defaultGenerateOps
          { modelName = "gemma3" -- TODO: Make this configurable
          , prompt = ""
          , stream = Just (T.putStr . Ollama.response_, pure ())
          }



plugin :: Plugin
plugin = defaultPlugin
    { holeFitPlugin = \_ -> Just $ HoleFitPluginR
        { hfPluginInit = newTcRef ()
        , hfPluginStop = \_ -> return ()
        , hfPluginRun = const HoleFitPlugin
            { candPlugin = \_ c -> return c -- Don't filter candidates
            , fitPlugin = \hole fits -> do
                dflags <- getDynFlags
                gbl_env <- getGblEnv
                let mod_name = moduleNameString $ moduleName $ tcg_mod gbl_env
                    imports = tcg_imports gbl_env

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
                    let prompt =
                         "You are a typed-hole plugin within GHC, the Glasgow Haskell Compiler.\n" ++
                         "You are given a hole in a Haskell program, and you need to fill it in.\n" ++
                         "The hole is represented by the following information:\n" ++
                          mn ++ "\n" ++ lc ++ "\n" ++ im ++ "\n" ++ hv ++ "\n" ++ ht ++ "\n" ++ rc ++ "\n" ++ le ++ "\n" ++ ge ++ "\n" ++ cf
                    let prompt' = prompt ++ "\n\n" ++ "Please provide a few candidate Haskell expression to fill in the hole:\n"
                    liftIO $ putStrLn $ "--- Ollama Prompt ---\n" ++ prompt' ++ "\n---------------------"
                    liftIO $ void $ generate genOps { prompt = prompt' }

                  Nothing -> liftIO $ putStrLn "Hole details not available (e.g., out of scope variable)."

                liftIO $ putStrLn "--- Ollama Plugin: End Hole ---"
                -- Return the original fits without modification
                return fits
            }
        }
    }
