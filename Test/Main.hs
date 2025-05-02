{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin=GHC.Plugin.OllamaHoles #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:model=qwen3:latest #-}
{-# OPTIONS_GHC "-fplugin-opt=GHC.Plugin.OllamaHoles:model-options={\"num_ctxt\": 32000, \"temperature\": 1.0}" #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:n=10 #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:debug #-}

module Main where

import qualified Data.List as L
import Data.Proxy
import GHC.TypeError

main :: IO ()
main = do
  let _guide = Proxy :: Proxy (Text "The function should sort the list and then show each element")
  let k = (_b :: [Int] -> [String])
  print (k [1, 2, 3])
