{-# LANGUAGE OverloadedStrings #-}

module TraceExample
  ( runTrace
  ) where

import Debug.Trace
  ( trace
  , traceIO
  )

add :: Int -> Int -> Int
add a b = trace (show a ++ " " ++ show b) (a + b)

ioAction :: Int -> IO Int
ioAction a = do
  traceIO (show a)
  pure a

runTrace :: IO ()
runTrace = do
  print $ add 11 22
  n <- ioAction 10
  print n
