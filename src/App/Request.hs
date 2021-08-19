{-# LANGUAGE OverloadedStrings #-}

module App.Request where

import qualified Network.HTTP.Simple as N
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad (mzero)
import Data.Aeson

import App.Config


data ReqText = ReqText { updateIdT :: Int 
                       , justIdT :: Int 
                       , messageT :: T.Text 
                       } deriving (Show, Eq)

data ReqButton = ReqButton  { updateIdB :: Int
                            , justIdB :: Int
                            , countB :: T.Text
                            } deriving (Show, Eq)

data ReqOther = ReqOther  { updateIdO :: Int
                          , justIdO :: Int
                          , fileIdO :: T.Text
                          } deriving (Show, Eq)

newtype ReqBool = ReqBool () deriving (Show, Eq) 


makeReqUpdates :: Token -> String
makeReqUpdates (Token token) = "https://api.telegram.org/bot" ++ token ++ "/getUpdates" ++ "?timeout=30"

getUpdates :: Token -> IO B.ByteString 
getUpdates token = do
  resp <- N.httpBS $ N.parseRequest_ $ makeReqUpdates token 
  return $ N.getResponseBody resp 




