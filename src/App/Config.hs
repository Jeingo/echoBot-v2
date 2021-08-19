{-# LANGUAGE OverloadedStrings #-}

module App.Config where

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Data.Text as T

data ConfData = ConfData { helpText :: String
                         , repeatText :: String
                         , startRepeat :: Int 
                         , button :: String
                         , token :: String
                         } deriving (Show)


readConfig :: IO CT.Config 
readConfig = C.load [C.Required "src/config/confBot.cfg"]

readToken :: IO String 
readToken = do
  con <- readFile "src/cfg/token"
  return $ filter (/= '\n') con

makeMyConfig :: CT.Config -> IO ConfData
makeMyConfig conf = do
  hT <- C.require conf (T.pack "main.helpText") :: IO String
  rT <- C.require conf (T.pack "main.repeatText") :: IO String
  sR <- C.require conf (T.pack "main.startRepeat") :: IO Int 
  bt <- C.require conf (T.pack "main.button") :: IO String
  tok <- readToken
  return $ ConfData hT rT sR bt tok

