-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import System.Environment

-- | Chat server entry point.
chat :: IO ()
chat = do
     chatPort <- getEnv "CHAT_SERVER_PORT"
     putStrLn chatPort
