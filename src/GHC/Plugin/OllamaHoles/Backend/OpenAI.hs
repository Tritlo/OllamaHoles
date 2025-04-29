{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The OpenAI backend
module GHC.Plugin.OllamaHoles.Backend.OpenAI (openAIBackend) where

import Data.Maybe (fromMaybe)
import Network.HTTP.Req
import System.Environment (lookupEnv)

import Data.Aeson (FromJSON (..), Value, object, parseJSON, (.:), (.=))
import Data.Aeson.Types (Parser, parseMaybe)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Plugin.OllamaHoles.Backend

-- | The OpenAI backend
openAIBackend :: Backend
openAIBackend = Backend{..}
  where
    listModels = do
        apiKey <- lookupEnv "OPENAI_API_KEY"
        case apiKey of
            Nothing -> return Nothing
            Just key -> do
                let url = https "api.openai.com" /: "v1" /: "models"
                    headers = header "Authorization" ("Bearer " <> encodeUtf8 (T.pack key))

                response <- runReq defaultHttpConfig $ req GET url NoReqBody jsonResponse headers
                return $ Just $ parseOpenAIModels (responseBody response)

    parseOpenAIModels :: Value -> [Text]
    parseOpenAIModels value =
        fromMaybe [] (parseMaybe parseModels value)
      where
        parseModels :: Value -> Parser [Text]

        extractModelId :: Value -> Parser Text
        extractModelId model = do
            obj <- parseJSON model
            obj .: "id"
        parseModels val = do
            obj <- parseJSON val
            models <- obj .: "data"
            mapM extractModelId models

    parseOpenAIResponse :: Value -> Maybe Text
    parseOpenAIResponse = parseMaybe parseResponse
      where
        parseResponse :: Value -> Parser Text
        parseResponse val = do
            obj <- parseJSON val
            choices <- obj .: "choices"
            case choices of
                [] -> fail "No choices in response"
                (choice : _) -> do
                    choiceObj <- parseJSON choice
                    message <- choiceObj .: "message"
                    messageObj <- parseJSON message
                    messageObj .: "content"

    generateFits prompt modelName = do
        apiKey <- lookupEnv "OPENAI_API_KEY"
        case apiKey of
            Nothing -> return $ Left "OpenAI API key not found. Set the OPENAI_API_KEY environment variable."
            Just key -> do
                let requestBody =
                        object
                            [ "model" .= modelName
                            , "messages" .= [object ["role" .= ("user" :: Text), "content" .= prompt]]
                            ]

                let url = https "api.openai.com" /: "v1" /: "chat" /: "completions"
                    headers =
                        header "Content-Type" "application/json"
                            <> header "Authorization" ("Bearer " <> encodeUtf8 (T.pack key))

                response <- runReq defaultHttpConfig $ req POST url (ReqBodyJson requestBody) jsonResponse headers

                case parseOpenAIResponse (responseBody response) of
                    Just content -> return $ Right content
                    Nothing ->
                        return $ Left $ "Failed to parse OpenAI response: " <> show (responseBody response)
