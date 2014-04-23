-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import Network (PortID(..), PortNumber, Socket, listenOn, accept)
import System.IO (BufferMode(..), Handle, utf8, hSetBuffering, hGetLine, hPutStrLn, hSetEncoding, hClose)
import System.Environment (getEnv)
import Control.Monad (forever)
import Control.Concurrent (forkIO, forkFinally, killThread)
import Control.Concurrent.Chan (Chan, newChan, dupChan, writeChan, readChan) 
import Control.Exception (IOException, catch)

-- Type alias: a user's identity is fully specified by their integer ID.
type UserID = Integer

-- A message can be a join notification, a message from a user to the chatroom, or a departure
-- notification. 
data Message = Join UserID | Msg UserID String | Leave UserID
instance Show Message where
    show (Join i) = show i ++ " has joined"
    show (Msg i m) = show i ++ ": " ++ m
    show (Leave i) = show i ++ " has left"

-- The name of the environment variable which stores the port we want to read.
portName :: String
portName = "CHAT_SERVER_PORT"

-- The main function of the module; starts a chat server on the given port.
chat :: IO ()
chat = do
    chatPort <- getEnv portName
    let portNum = (fromIntegral :: Integer -> PortNumber) $ read chatPort
    socket <- listenOn $ PortNumber portNum

    chan <- newChan
    _ <- forkIO $ forever $ readChan chan -- If the original channel is not read from, it accumulates excess memory; this loop just clears it out

    handleNewConnections chan socket 1

-- Handle new incoming connections on the given socket and register them as a new client with the
-- given UserID.
handleNewConnections :: Chan Message -> Socket -> UserID -> IO ()
handleNewConnections chan socket i = do
    (client, _, _) <- accept socket
    hSetBuffering client NoBuffering
    hSetEncoding client utf8

    chan2 <- dupChan chan
    _ <- forkFinally (serveChats client chan2 i) (\_ -> hClose client)
    let nextID = i+1
    handleNewConnections chan socket $! nextID

-- Serve chats for each 
serveChats :: Handle -> Chan Message -> UserID -> IO ()
serveChats client chan i = do
        chan2 <- dupChan chan
        writeChan chan (Join i)
        tid <- forkIO $ printClientLoop client chan2 i
        handleClientInput client chan i `catch` \e -> do 
            let _ = (e :: IOException)
            killThread tid
            writeChan chan (Leave i)

-- Handle client input and pass it on to the given channel.
handleClientInput :: Handle -> Chan Message -> UserID -> IO ()
handleClientInput client chan i = forever $ do
    line <- hGetLine client
    writeChan chan (Msg i line)

-- Read messages from the given channel, and print them out if they are not one's own.
printClientLoop :: Handle -> Chan Message -> UserID -> IO ()
printClientLoop client chan i = forever $ do
    msg <- readChan chan
    case msg of
        (Msg j _) | j == i -> return ()
        _ -> hPutStrLn client $ show msg
