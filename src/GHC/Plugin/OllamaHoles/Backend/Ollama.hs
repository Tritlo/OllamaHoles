{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The locally hosted ollama backend
module GHC.Plugin.OllamaHoles.Backend.Ollama (ollamaBackend) where

import Ollama (GenerateOps (..))
import Ollama qualified

import GHC.Plugin.OllamaHoles.Backend

-- | The locally hosted ollama backend
ollamaBackend :: Backend
ollamaBackend = Backend{..}
  where
    listModels = fmap getMs <$> Ollama.list
    getMs (Ollama.Models models) = fmap Ollama.name models
    generateFits prompt modelName = do
        fmap Ollama.response_ <$> Ollama.generate genOps{prompt = prompt, modelName = modelName}

    -- \| Options for the LLM model
    genOps :: Ollama.GenerateOps
    genOps =
        Ollama.defaultGenerateOps
            { modelName = ""
            , prompt = ""
            }
