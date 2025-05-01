{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fplugin=GHC.Plugin.OllamaHoles #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:model=gemma3:27b-it-qat #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:n=5 #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:debug #-}

module Main where

import qualified Data.List as L

import GHC.TypeError
import Data.Proxy

main :: IO ()
main = do let _guide = Proxy :: Proxy (Text "The function should sort the list and then show each int")
          let k = (_b :: [Int] -> [String])
          print (k [1,2,3])
    
