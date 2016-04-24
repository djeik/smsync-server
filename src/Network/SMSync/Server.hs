{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.SMSync.Server where

import qualified Network.SMSync.Server.Parser as P
import Network.SMSync.Types

import Control.Monad ( forever )
import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent ( forkIO, killThread )
import Control.Concurrent.MVar
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import qualified Data.Map as M
import Data.Maybe ( listToMaybe, catMaybes )
import Data.Monoid ( (<>) )
import qualified Data.Time as T
import Data.Word
import Network.Socket
import System.IO
import Text.Read ( readMaybe )

data ServerSettings
    = ServerSettings
        { serverHost :: String
        , serverPort :: Int
        }
    deriving (Eq, Ord, Read, Show)

defaultServerSettings = ServerSettings
    { serverHost = "0.0.0.0"
    , serverPort = 7777
    }

newtype ServerT m a
    = ServerT
        { runServerT
            :: ExceptT ServerError (
                ReaderT ServerEnv m
            )
            a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader ServerEnv
        , MonadError ServerError
        , MonadIO
        )

type Server = ServerT IO

data ServerEnv
    = ServerEnv
        { _phoneMap :: MVar PhoneMap
        , _socketHandle :: Handle
        , _clientId :: Int
        }

data ServerError
    = Inconsistency String
    deriving (Eq, Ord, Read, Show)

-- | Logs a message indicating input received from a client.
echoIn :: (MonadReader ServerEnv m, MonadIO m) => BS.ByteString -> m ()
echoIn b = do
    cid <- asks _clientId
    liftIO $ do
        C8.putStr "#"
        C8.putStr (C8.pack $ show cid)
        C8.putStr " > "
        C8.putStrLn b

-- | Logs a message indicating output send to a client.
echoOut :: (MonadReader ServerEnv m, MonadIO m) => BS.ByteString -> m ()
echoOut b = do
    cid <- asks _clientId
    liftIO $ do
        C8.putStr "#"
        C8.putStr (C8.pack $ show cid)
        C8.putStr " < "
        C8.putStrLn b

-- | Reads a line from the client.
sGetLine :: (MonadReader ServerEnv m, MonadIO m) => m BS.ByteString
sGetLine = do
    h <- asks _socketHandle
    l <- liftIO $ C8.hGetLine h
    echoIn l
    pure l

-- | Writes a line to the client.
sPutStrLn :: (MonadReader ServerEnv m, MonadIO m) => BS.ByteString -> m ()
sPutStrLn l = do
    h <- asks _socketHandle
    echoOut l
    liftIO $ C8.hPutStrLn h l

-- | Writes a string to the client.
sPutStr :: (MonadReader ServerEnv m, MonadIO m) => BS.ByteString -> m ()
sPutStr l = do
    h <- asks _socketHandle
    echoOut l
    liftIO $ C8.hPutStr h l

-- | Reinterprets a parser in an exception monad
runParseOnly
    :: MonadError e m
    => (String -> e) -- ^ The error to throw in case of parse failure
    -> Parser a -- ^ The parser to run
    -> BS.ByteString -- ^ The input to run the parser on
    -> m a -- ^ The parser reinterpreted in the exception monad
runParseOnly f p l = case parseOnly p l of
    Left e -> throwError (f e)
    Right x -> pure x

-- | Runs an SMSP server with the given settings.
-- The server is run in a separate thread while the main thread blocks trying
-- to read a line from standard in. When the call to 'getLine' completes, the
-- server socket is closed, killing the server.
server :: ServerSettings -> IO ()
server (ServerSettings { serverPort = port, serverHost = host }) = do
    -- set up the server socket
    serverSocket <- socket AF_INET Stream defaultProtocol
    bind serverSocket . SockAddrInet (fromIntegral port) =<< inet_addr host
    listen serverSocket 5

    -- create some dummy data because we don't have a database yet
    clientMap <- newMVar $
        M.fromList
            [ ( "XXX"
              , PhoneInfo
                { phoneNumber = "5145033100"
                , phoneOwner = Just "Jake"
                }
              )
            ]

    -- set up a counter for client IDs
    clientIdRef <- newIORef 0
    let nextId = readIORef clientIdRef <* modifyIORef clientIdRef (+1)

    -- run the main server loop in another thread
    tid <- forkIO $ forever $ do
        -- block until we get a connection
        (clientSocket, clientAddr) <- accept serverSocket
        -- get a handle from the socket so we can do read/write
        h <- socketToHandle clientSocket ReadWriteMode
        clientId <- nextId
        -- run the connection handler in another thread
        forkIO $ handleConnection clientId h clientMap

    -- block so we can gracefully shutdown
    getLine
    killThread tid
    close serverSocket

handleConnection :: Int -> Handle -> MVar PhoneMap -> IO ()
handleConnection clientId h mpm = do
    status <- runReaderT
        (runExceptT (runServerT preHello))
        (ServerEnv { _socketHandle = h, _phoneMap = mpm, _clientId = clientId })

    case status of
        Left e -> do
            putStrLn $ "#" ++ show clientId ++ " failed:" ++ show e
        Right () -> do
            putStrLn $ "#" ++ show clientId ++ " quit successfully"

preHello :: Server ()
preHello = do
    line <- sGetLine
    runParseOnly
        (Inconsistency . ("invalid greeting: " ++))
        (P.greeting <* endOfInput)
        line
    sPutStrLn "Hello"
    preAuth

preAuth :: Server ()
preAuth = do
    line <- sGetLine

    key <- runParseOnly
        (Inconsistency . ("invalid auth: " ++))
        (P.auth <* endOfInput)
        line

    mpm <- asks _phoneMap
    pm <- liftIO $ readMVar mpm

    case M.lookup key pm of
        Nothing -> do
            sPutStrLn "BadKey"
            preAuth
        Just phoneInfo -> do
            sPutStrLn "Hello"
            preMode phoneInfo

preMode = undefined

