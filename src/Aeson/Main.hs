{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
  ( (^.)
  , makeLenses
  )
import Control.Monad
  ( mzero )
import Data.Aeson
  ( (.:)
  , (.:?)
  , (.=)
  , FromJSON
  , ToJSON
  , Value(Object)
  , decode
  , encode
  , object
  , parseJSON
  , toJSON
  )
import Data.Maybe
  ( fromMaybe )

data User = User
  { _userName   :: String
  , _userEmail  :: String
  , _userFavNum :: Maybe Int
  } deriving (Show, Eq)

makeLenses ''User

-- turn a User into a JSON object
instance ToJSON User where
  toJSON u =
    object [ "name"    .= (u ^. userName)
           , "email"   .= (u ^. userEmail)
           , "fav_num" .= fromMaybe 0 (u ^. userFavNum)
           ]

-- turn a JSON object into a User
instance FromJSON User where
  parseJSON (Object v) = User
    <$> v .:  "name"
    <*> v .:  "email"
    <*> v .:? "fav_num" -- this could be empty, so parse as Maybe
  parseJSON _ = mzero

runAeson :: IO ()
runAeson = do
  print u
  print (encode u)
  -- note that decoding results in a Maybe
  -- we can't be sure correct data was passed
  print (decode j :: Maybe User)
  where
    u = User "George" "george@example.com" Nothing
    j = "{\"name\":\"Jack\",\"email\":\"jack@example.com\",\"fav_num\":7}"

main :: IO ()
main = runAeson
