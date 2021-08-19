module Main where

import App.Config
import App.Request

main :: IO ()
main = do
  myConfigTmp <- readConfig
  myConfig <- makeMyConfig myConfigTmp
  print myConfig
  return ()

