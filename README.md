# Ollama Holes

![image](https://github.com/user-attachments/assets/649ffcd2-0560-47d6-bbbe-74bae08cbb70)

## Introduction
This is an example of a typed-hole plugin for GHC that uses the [Ollama](https://ollama.com/) LLM to fill in holes in Haskell code.


## Example
Given 

```haskell
{-# OPTIONS_GHC -fplugin=GHC.Plugin.OllamaHoles #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:model=gemma3:27b #-}
{-# OPTIONS_GHC -fplugin-opt=GHC.Plugin.OllamaHoles:n=5 #-}

module Main where

import Data.List


main :: IO ()
main = do let k = (_b :: [Int] -> [String])
          print (k [1,2,3])

```

We get the following output:


```text
    Main.hs:12:20: error: [GHC-88464]
        • Found hole: _b :: [Int] -> [String]
        Or perhaps ‘_b’ is mis-spelled, or not in scope
        • In the expression: _b :: [Int] -> [String]
        In an equation for ‘k’: k = (_b :: [Int] -> [String])
        In the expression:
            do let k = (_b :: [Int] -> [String])
            print (k [1, 2, ....])
        • Relevant bindings include
            k :: [Int] -> [String] (bound at Main.hs:12:15)
            main :: IO () (bound at Main.hs:12:1)
        Valid hole fits include
            \x -> map show x
            map (show)
            \x -> map (\y -> "Number: " ++ show y) x
            \x -> replicate (length x) "Hello"
            []
    |
    12 | main = do let k = (_b :: [Int] -> [String])
    |                    ^^
    Error: cabal: Failed to build exe:main from OllamaHolesTest-1.0.0.
```

## Installation

1. Install [Ollama](https://ollama.com/docs/installation)
2. Install the `gemma3:27b` model (or any other model you prefer) using the following command:

```bash

ollama pull gemma3:27b
```
3. Clone this repository and navigate to the directory, and build the project using:

```bash
cabal build
```
4. Run the example using:

```bash
cabal build Test
