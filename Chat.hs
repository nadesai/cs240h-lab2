-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import System.Environment

portName = "CHAT_SERVER_PORT"

-- | Chat server entry point.
chat :: IO ()
chat = do
     chatPort <- getEnv portName
     putStrLn chatPort

