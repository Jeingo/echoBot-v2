module Main where

import qualified Data.Map as Map

import App.Config
import App.Request
import App.MainLoop

main :: IO ()
main = do
  myConfigTmp <- readConfig
  myConfig <- makeMyConfig myConfigTmp
  mainLoop myConfig Map.empty
  return ()

