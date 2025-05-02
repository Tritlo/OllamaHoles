{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The Gemini backend
module GHC.Plugin.OllamaHoles.Backend.Gemini (geminiBackend) where

import Network.HTTP.Req
import System.Environment (lookupEnv)

import Data.Aeson (FromJSON (..), Value (..), object, parseJSON, (.:), (.=))
import Data.Aeson.Types (Parser, parseMaybe)

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Plugin.OllamaHoles.Backend
import Data.Maybe

-- | The Gemini backend
geminiBackend :: Backend
geminiBackend = Backend{..}
  where
    apiEndpoint = https "generativelanguage.googleapis.com"  /: "v1beta"
    listModels = do
        apiKey <- lookupEnv "GEMINI_API_KEY"
        case apiKey of
            Nothing -> return Nothing
            Just key -> do
                let url = apiEndpoint /: "models"
                response <- runReq defaultHttpConfig $ req GET url NoReqBody jsonResponse ("key" =: key)
                return $ Just $ parseGeminiModels (responseBody response)

    parseGeminiResponse :: Value -> Maybe Text
    parseGeminiResponse = parseMaybe parseResponse
      where
        parseResponse :: Value -> Parser Text
        parseResponse val = do
            obj <- parseJSON val
            candidates <- obj .: "candidates"
            case candidates of
                [] -> fail "No candidates in response"
                (candidate : _) -> do
                    candidateObj <- parseJSON candidate
                    content <- candidateObj .: "content"
                    parts <- content .: "parts"
                    case parts of
                        [] -> fail "No parts in content"
                        (part : _) -> do
                            partObj <- parseJSON part
                            partObj .: "text"

    generateFits prompt modelName options = do
        apiKey <- lookupEnv "GEMINI_API_KEY"
        case apiKey of
            Nothing -> return $ Left "Gemini API key not found. Set the GEMINI_API_KEY environment variable."
            Just key -> do
                let base_req =
                        object
                            [ "contents" .= [object ["parts" .= [object ["text" .= prompt]]]]
                            ]
                    requestBody = case options of
                                    Just (Object opts) | Object base <- base_req -> Object (base <> opts)
                                    _ -> base_req
                let url = apiEndpoint /: "models" /: (modelName <>  ":generateContent")
                    params = "key" =: key
                    headers = header "Content-Type" "application/json"

                response <- runReq defaultHttpConfig $ req POST url (ReqBodyJson requestBody) jsonResponse (headers <> params)

                case parseGeminiResponse (responseBody response) of
                    Just content -> return $ Right content
                    Nothing ->
                        return $ Left $ "Failed to parse Gemini response: " <> show (responseBody response)

-- | Parse the models from the endpoint
parseGeminiModels :: Value -> [Text]
parseGeminiModels value =
    fromMaybe [] (parseMaybe parseModels value)
  where
    extractModelId :: Value -> Parser Text
    extractModelId model = do
        obj <- parseJSON model
        T.drop (T.length "models/") <$> obj .: "name"

    parseModels :: Value -> Parser [Text]
    parseModels val = do
        obj <- parseJSON val
        models <- obj .: "models"
        mapM extractModelId models
