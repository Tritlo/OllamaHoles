{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The OpenAI backend
module GHC.Plugin.OllamaHoles.Backend.OpenAI (openAIBackend, openAICompatibleBackend) where

import Network.HTTP.Req
import System.Environment (lookupEnv)

import Data.Aeson (FromJSON (..), Value, object, parseJSON, (.:), (.=))
import Data.Aeson.Types (Parser, parseMaybe)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Plugin.OllamaHoles.Backend
import Data.Maybe (fromMaybe)
import Text.URI (mkURI)


-- | The OpenAI backend
openAIBackend :: Backend
openAIBackend = openAICompatibleBackend "api.openai.com" "OPENAI_API_KEY"

-- | Any OpenAI compatible backend
openAICompatibleBackend :: Text -> Text -> Backend
openAICompatibleBackend base_url key_name = Backend {..}
  where
    apiEndpoint = do uri <- mkURI base_url
                     case useHttpsURI uri of
                      Just (url, opts) -> return (url /: "v1", opts)
                      Nothing -> error $ "could not parse " <> T.unpack base_url <> " as a URI"
    listModels = do
        apiKey <- lookupEnv $ T.unpack key_name
        case apiKey of
            Nothing -> return Nothing
            Just key -> do
                (url, opts) <-  apiEndpoint
                let headers = header "Authorization" ("Bearer " <> encodeUtf8 (T.pack key))
                response <- runReq defaultHttpConfig $ req GET (url /: "models") NoReqBody jsonResponse (headers <> opts)
                return $ Just $ parseOpenAIModels (responseBody response)

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
        apiKey <- lookupEnv $ T.unpack key_name
        case apiKey of
            Nothing -> return $ Left $ "API key not found. Set the " <> T.unpack key_name <> " environment variable."
            Just key -> do
                let requestBody =
                        object
                            [ "model" .= modelName
                            , "messages" .= [object ["role" .= ("user" :: Text), "content" .= prompt]]
                            ]

                (url, opts) <- apiEndpoint
                let headers =
                        header "Content-Type" "application/json"
                            <> header "Authorization" ("Bearer " <> encodeUtf8 (T.pack key))

                response <- runReq defaultHttpConfig $
                              req POST (url /: "chat" /: "completions") (ReqBodyJson requestBody) jsonResponse (headers <> opts)

                case parseOpenAIResponse (responseBody response) of
                    Just content -> return $ Right content
                    Nothing ->
                        return $ Left $ "Failed to parse OpenAI response: " <> show (responseBody response)

-- | Parse the models from the endpoint
parseOpenAIModels :: Value -> [Text]
parseOpenAIModels value =
    fromMaybe [] (parseMaybe parseModels value)
  where
    extractModelId :: Value -> Parser Text
    extractModelId model = do
        obj <- parseJSON model
        obj .: "id"

    parseModels :: Value -> Parser [Text]
    parseModels val = do
        obj <- parseJSON val
        models <- obj .: "data"
        mapM extractModelId models
