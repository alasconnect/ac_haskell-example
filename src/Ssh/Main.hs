module Main where

import Network.SSH.Client.SimpleSSH
import System.IO

runSsh :: IO ()
runSsh = do
  s <- runSimpleSSH $ openSession "localhost" 22 "~/.ssh/known_hosts"
  case s of
    Left e  -> putStrLn $ show e
    Right r -> terminal r

terminal :: Session -> IO ()
terminal s = undefined

main :: IO ()
main = runSsh
