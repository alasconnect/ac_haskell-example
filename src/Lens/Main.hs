{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
  ( (^.)
  , (&)
  , (.~)
  , (%~)
  , makeLenses
  , over
  , set
  , view
  )

type Zip = Int

data Address = Address
  { _addressCity  :: String
  , _addressState :: String
  , _addressZip   :: Zip
  } deriving (Show)

makeLenses ''Address

mkAddress :: String -> String -> Zip -> Address
mkAddress = Address

data User = User
  { _userFirstName :: String
  , _userLastName  :: String
  , _userAddress   :: Address
  } deriving (Show)

makeLenses ''User

mkUser :: String -> String -> Address -> User
mkUser = User

runLens :: IO ()
runLens = do
  -- print user with updated first and last names
  print u2

  -- infix getters -- looks like OO
  print ("City: " ++ u2 ^. userAddress . addressCity)
  print ("State: " ++ u2 ^. userAddress . addressState)
  -- named getter -- allows for composition
  print ("Zip: " ++ show (view (userAddress . addressZip) u2))

  -- show setter composition
  print (u3 "Jane" "Doe" (mkAddress "Anchorage" "AK" 99999))

  -- show map over
  print u4
  print u5
  where
    -- smart constructors
    u0 = mkUser "Jim" "Bob" (mkAddress "Fairbanks" "AK" 99701)

    -- set equivalents
    u1 = u0 & userFirstName .~ "Jimmy" -- infix
    u2 = set userLastName "Bobby" u1
    -- composition
    u3 f l a = set userFirstName f . set userLastName l . set userAddress a $ u2

    -- map examples
    u4 = over userAddress (\a -> a & addressZip .~ 88888) u2
    -- infix syntax partially applying the address
    u5 = u4 & userAddress %~ set addressState "WA"

main :: IO ()
main = runLens
