{-# LANGUAGE OverloadedStrings #-}

module App.MainLoop where

import qualified Data.Map as Map
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B8
import qualified Data.Text as T
import Data.Maybe

import App.Config
import App.Request

type CountUser = Map.Map Int Int

mainLoop :: ConfData -> CountUser -> IO ()
mainLoop conf allUsers = do 
  let token = getToken conf
  responseTmp <- getUpdates token
  let typeResp = switcher responseTmp  
  case typeResp of

    "empty" -> mainLoop conf allUsers
    
    "text" -> do
      let response = fromJust (makeResponse responseTmp)
      let chatIdTmp = justIdT response
      let users = addNewUser allUsers chatIdTmp (startRepeat conf)
      case (messageT response) of
        "/help" -> sendHelpText (helpText conf) (show chatIdTmp) token 
        "/repeat" -> sendKeyboard (B8.fromString $ button conf) (show chatIdTmp) token (repeatText conf)
        _ -> sendEcho (fromJust $ Map.lookup chatIdTmp users) token response
      nextStep (show $ (updateIdT response) + 1) token
      mainLoop conf users 

    "button" -> do
      let response = fromJust (makeResponse responseTmp)
      let chatIdTmp = justIdB response
      let newCount = read $ T.unpack (countB response) :: Int
      let users = Map.insert chatIdTmp newCount allUsers
      nextStep (show $ (updateIdB response) + 1) token
      mainLoop conf users

    "other" -> do
      let response = fromJust (makeResponse responseTmp)
      let chatIdTmp = justIdS response
      let users = addNewUser allUsers chatIdTmp (startRepeat conf)
      sendEcho (fromJust $ Map.lookup chatIdTmp users) token response
      nextStep (show $ (updateIdS response) + 1) token
      mainLoop conf users

  return ()

-- test main func
chooseRoad :: String -> IO ()
chooseRoad switch 
  | switch == "empty" = emptyFunc
  | switch == "text" = textFunc
  | switch == "button" = buttonFunc
  | switch == "other" = otherFunc
  | otherwise = otherwiseFunc

emptyFunc = undefined
textFunc = undefined
buttonFunc = undefined
otherFunc = undefined
otherwiseFunc = undefined

-- end testing

switcher :: B.ByteString -> String 
switcher resp 
  | (makeResponse resp :: Maybe ReqBool) /= Nothing = "empty"
  | (makeResponse resp :: Maybe ReqText) /= Nothing = "text"
  | (makeResponse resp :: Maybe ReqButton) /= Nothing = "button"
  | (makeResponse resp :: Maybe ReqStiker) /= Nothing = "other"

addNewUser :: CountUser -> Int -> Int -> CountUser
addNewUser arr newUser startRepeat = if Map.member newUser arr
                                        then arr
                                        else Map.insert newUser startRepeat arr

