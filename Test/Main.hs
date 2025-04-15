{-# OPTIONS_GHC -fplugin=GHC.Plugin.OllamaHoles #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:model=gemma3:27b #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:n=5 #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:debug=True #-}

module Main where

import Data.List


main :: IO ()
main = do let k = (_b :: [Int] -> [String])
          print (k [1,2,3])
    
