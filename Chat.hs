-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import Network (listenOn, accept, PortID(..), PortNumber, HostName, Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import System.Environment (getEnv)

type UserID = Integer

data Message = Join UserID | Msg UserID String | Leave UserID
instance Show Message where
    show (Join i) = show i ++ " has joined"
    show (Msg i m) = show i ++ ": " ++ m
    show (Leave i) = show i ++ " has left"

-- import Chat
portName :: String
portName = "CHAT_SERVER_PORT"

chat :: IO ()
chat = do
    putStrLn $ "Server started."
    chatPort <- getEnv portName
    let portNum = (fromIntegral :: Integer -> PortNumber) $ read chatPort
    sock <- listenOn $ PortNumber portNum

    chan <- newChan
    _ <- forkIO $ printLog chan
    handleNewConnections chan sock 0

printLog :: Chan Message -> IO ()
printLog chan = do
    msg <- readChan chan
    putStrLn $ show msg
    printLog chan

handleNewConnections :: Chan Message -> Socket -> UserID -> IO ()
handleNewConnections chan sock i = do
    -- accept client as handle
    (client, h, n) <- accept sock
    hSetBuffering client NoBuffering

    -- create new channel to communicate with client
    newChan <- dupChan chan
    -- send this channel to the client handler
    _ <- forkIO $ serveChats client newChan (h,n) i
    handleNewConnections chan sock $! (i+1)

serveChats :: Handle -> Chan Message -> (HostName, PortNumber) -> UserID -> IO ()
serveChats client chan (h,n) i =
    do
        -- putStrLn $ "Client on " ++ h ++ ":" ++ show n ++ " connected"
        writeChan chan (Join i)
        -- hPutStrLn client $ show (Join i)
        newChan <- dupChan chan
        _ <- forkIO $ handleClientInput client chan i
        printClientLoop client chan i

handleClientInput :: Handle -> Chan Message -> UserID -> IO ()
handleClientInput client chan i = do
    line <- hGetLine client
    let msg = Msg i line
    writeChan chan msg
    -- putStrLn $ show msg
    hPutStrLn client $ show msg
    handleClientInput client chan i

printClientLoop :: Handle -> Chan Message -> UserID -> IO ()
printClientLoop client chan i = do
    msg <- readChan chan
    hPutStrLn client $ show msg
    printClientLoop client chan i
