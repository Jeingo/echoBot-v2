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


--Get Updates

urlApiTelegram :: String
urlApiTelegram = "https://api.telegram.org/bot"

getUpdates :: Token -> IO B.ByteString 
getUpdates (Token token) = do
  let req = urlApiTelegram ++ token ++ "/getUpdates" ++ "?timeout=30"
  resp <- N.httpBS $ N.parseRequest_ req 
  return $ N.getResponseBody resp 


--Parser Response

class Response a where
  makeResponse :: B.ByteString -> Maybe a
  
instance Response ReqText where
  makeResponse resp = decodeStrict resp :: Maybe ReqText

instance Response ReqButton where
  makeResponse resp = decodeStrict resp :: Maybe ReqButton

instance Response ReqOther where
  makeResponse resp = decodeStrict resp :: Maybe ReqOther

instance Response ReqBool where
  makeResponse resp = decodeStrict resp :: Maybe ReqBool

instance FromJSON ReqText where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    mesg <- mes .: "text" 
    from <- mes .: "from"
    jId <- from .: "id"
    return $ ReqText upId jId mesg 

  parseJSON _ = mzero

instance FromJSON ReqOther where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    stick <- mes .: "sticker" 
    from <- mes .: "from"
    jId <- from .: "id"
    fId <- stick .: "file_id"
    return $ ReqOther upId jId fId 

  parseJSON _ = mzero

instance FromJSON ReqButton where
  parseJSON (Object req) = do
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    callback <- arr .: "callback_query"
    count <- callback .: "data"
    from <- callback .: "from"
    jId <- from .: "id"
    return $ ReqButton upId jId count

  parseJSON _ = mzero


instance FromJSON ReqBool where
  parseJSON (Object req) = do
    result <- req .: "result"
    return $ ReqBool result

  parseJSON _ = mzero


--Offset function

type Offset = String

nextStep :: Offset -> Token -> IO ()
nextStep offset (Token token) = do
  let req = urlApiTelegram ++ token ++ "/getUpdates" ++ "?offset=" ++ offset
  N.httpNoBody $ N.parseRequest_ req
  return () 


--Sending function

type HelpText = String
type ChatId = String

sendHelpText :: HelpText -> ChatId -> Token -> IO ()
sendHelpText helpT chatId (Token token) = do
  let req = urlApiTelegram ++ token ++ "/sendMessage" ++ "?chat_id=" ++ chatId ++ "&text=" ++ helpT 
  N.httpNoBody $ N.parseRequest_ $ req
  return ()

type Counter = Int
type Date = String

{--
sendEcho :: Counter -> Date -> Token -> ChatId -> IO ()
sendEcho counter date (Token token) = do
  let dateAndCount = replicate counter date
  let req = sendDate token chatId date
--}

















