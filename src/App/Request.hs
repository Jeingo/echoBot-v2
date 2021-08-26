{-# LANGUAGE OverloadedStrings #-}

module App.Request where

import qualified Network.HTTP.Simple as N
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad 
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

data ReqStiker = ReqStiker  { updateIdS :: Int
                            , justIdS :: Int
                            , fileIdS :: T.Text
                            } deriving (Show, Eq)

newtype ReqBool = ReqBool () deriving (Show, Eq) 

--Get Updates

urlApiTelegram :: String
urlApiTelegram = "https://api.telegram.org/bot"

getUpdates :: Token -> IO B.ByteString 
getUpdates (Token token) = do
  let req = urlApiTelegram ++ token ++ "/getUpdates" ++ "?timeout=29"
  resp <- N.httpBS $ N.parseRequest_ req 
  return $ N.getResponseBody resp 


--Parser Response

class Response a where
  makeResponse :: B.ByteString -> Maybe a
  
instance Response ReqText where
  makeResponse resp = decodeStrict resp :: Maybe ReqText

instance Response ReqButton where
  makeResponse resp = decodeStrict resp :: Maybe ReqButton

instance Response ReqStiker where
  makeResponse resp = decodeStrict resp :: Maybe ReqStiker

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

instance FromJSON ReqStiker where
  parseJSON (Object req) = do 
    result <- req .: "result"
    let arr = V.head result
    upId <- arr .: "update_id"
    mes <- arr .: "message"
    stick <- mes .: "sticker" 
    from <- mes .: "from"
    jId <- from .: "id"
    fId <- stick .: "file_id"
    return $ ReqStiker upId jId fId 

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

sendKeyboard :: B.ByteString -> ChatId -> Token -> String -> IO () 
sendKeyboard keyB chId (Token token) repeatText = do
  let repText = B8.fromString repeatText
  let chatId = B8.fromString chId 
  request' <- N.parseRequest $ "POST " ++ urlApiTelegram ++ token ++ "/sendMessage"
  let req = N.setRequestQueryString [("chat_id", Just $ chatId),
                                     ("text", Just $ repText), 
                                     ("reply_markup", Just keyB)] $ request'
  N.httpNoBody req
  return () 

type Counter = Int

class Sender a where
  makeSendReq :: Token -> a -> String

instance Sender ReqText where
  makeSendReq (Token token) resp = urlApiTelegram ++ token ++ "/sendMessage" ++ "?chat_id=" ++ chId ++ "&text=" ++ textResp
    where chId = show $ justIdT resp
          textResp = T.unpack $ messageT resp

instance Sender ReqStiker where
  makeSendReq (Token token) resp = urlApiTelegram ++ token ++ "/sendAnimation" ++ "?chat_id=" ++ chId ++ "&animation=" ++ fileId 
    where chId = show $ justIdS resp
          fileId = T.unpack $ fileIdS resp

sendEcho :: Sender a => Counter -> Token -> a -> IO ()
sendEcho counter token resp = do
  let req = makeSendReq token resp
  replicateM_ counter $ N.httpNoBody $ N.parseRequest_ req
  return ()

















