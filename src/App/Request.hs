{-# LANGUAGE OverloadedStrings #-}

module App.Request where

import qualified Network.HTTP.Simple as N
import qualified Data.ByteString.UTF8 as B8
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad (mzero)
import Data.Aeson


data InitReqText = InitRT { updateId :: Int 
                          , justId :: Int 
                          , message :: T.Text 
                          } deriving (Show, Eq)

data InitReqButton = InitRB  { updateIdB :: Int
                             , justIdB :: Int
                             , countB :: T.Text
                             } deriving (Show, Eq)

data InitReqOther = InitRO  { updateIdO :: Int
                            , justIdO :: Int
                            , fileId :: T.Text
                            } deriving (Show, Eq)

newtype BoolReq = BoolReq () deriving (Show, Eq) 
