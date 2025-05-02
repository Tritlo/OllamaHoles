{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin=GHC.Plugin.OllamaHoles #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:model=qwen3:32b #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:n=10 #-}

module Main where

import qualified Data.List as L

import GHC.TypeError
import Data.Proxy

main :: IO ()
main = do let _guide = Proxy :: Proxy (Text "The function should sort the list and then show each element")
          let k = (_b :: [Int] -> [String])
          print (k [1,2,3])

