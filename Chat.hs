-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import Network (listenOn, accept, PortID(..), PortNumber, Socket)
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
    (client, _, _) <- accept sock
    hSetBuffering client NoBuffering

    chan2 <- dupChan chan
    _ <- forkIO $ serveChats client chan2 i
    handleNewConnections chan sock $! (i+1)

serveChats :: Handle -> Chan Message -> UserID -> IO ()
serveChats client chan i =
    do
        writeChan chan (Join i)
        chan2 <- dupChan chan
        _ <- forkIO $ handleClientInput client chan2 i
        printClientLoop client chan i

handleClientInput :: Handle -> Chan Message -> UserID -> IO ()
handleClientInput client chan i = do
    line <- hGetLine client
    let msg = Msg i line
    writeChan chan msg
    handleClientInput client chan i

printClientLoop :: Handle -> Chan Message -> UserID -> IO ()
printClientLoop client chan i = do
    msg <- readChan chan
    case msg of
        (Msg j _) | j == i -> return ()
        _ -> hPutStrLn client $ show msg
    printClientLoop client chan i
