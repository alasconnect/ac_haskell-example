{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module ServantExample
    ( runServant
    ) where

import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

data User = User
  { _userName   :: String
  , _userEmail  :: String
  , _userFavNum :: Maybe Int
  } deriving (Show, Eq, Generic)

instance ToJSON User

type UserApi = "users" :> Get '[JSON] [User]

users :: [User]
users =
  [ User "Jessica Jones" "jj@marvel.com" (Just 5)
  , User "Luke Cage"     "lc@marvel.com" Nothing
  ]

server :: Server UserApi
server = pure users

userApi :: Proxy UserApi
userApi = Proxy

app :: Application
app = serve userApi server

runServant :: IO ()
runServant = run 8080 app
