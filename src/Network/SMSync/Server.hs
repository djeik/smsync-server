{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.SMSync.Server where

import Network.SMSync.Types
import Network.SMSync.Parser

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad ( forever )
import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Map as Map
import Data.Maybe ( listToMaybe, catMaybes )
import Data.Monoid ( (<>) )
import qualified Data.Time as T
import Data.Word
import Network.Socket
import System.IO
import System.IO.Unsafe ( unsafePerformIO )
import Text.Read ( readMaybe )

data ServerSettings
    = ServerSettings
        { serverBindHost :: String
        , serverBindPort :: PortNumber
        }

defaultSettings :: ServerSettings
defaultSettings
    = ServerSettings
        { serverBindHost = "0.0.0.0"
        , serverBindPort = 7777
        }

run :: IO ()
run = runWithSettings defaultSettings

runWithSettings :: ServerSettings -> IO ()
runWithSettings settings = do
    serverSocket <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr (serverBindHost settings)
    bind serverSocket $ SockAddrInet (serverBindPort settings) addr
    listen serverSocket 5
    clientMap <- newMVar $
        Map.fromList
            [ ( "XXX"
              , PhoneInfo
                { phoneNumber = "5145033100"
                , phoneOwner = Just "Jake"
                }
              )
            ]

    forkIO $ forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        h <- socketToHandle clientSocket ReadWriteMode
        putStrLn "Got connection"
        forkIO $ handleConnection h clientMap

    getLine
    close serverSocket

handleConnection :: Handle -> MVar PhoneMap -> IO ()
handleConnection h mpm = forever $ preHello h mpm

preHello :: Handle -> MVar PhoneMap -> IO ()
preHello h mpm = do
    let sPutStrLn l = C8.hPutStrLn h l *> echoOut l
    let sGetLine = C8.hGetLine h >>= \l -> echoIn l *> pure (l <> "\n")

    line <- sGetLine

    case parseOnly (greeting <* endOfInput) line of
        Left e -> do
            putStrLn e
            sPutStrLn "Bye"
        Right () -> do
            sPutStrLn "Hello"
            preAuth h mpm

preAuth :: Handle -> MVar PhoneMap -> IO ()
preAuth h mpm = do
    let sPutStrLn l = C8.hPutStrLn h l *> echoOut l
    let sGetLine = C8.hGetLine h >>= \l -> echoIn l *> pure (l <> "\n")

    line <- sGetLine

    case parseOnly (auth <* endOfInput) line of
        Left e -> do
            -- putStrLn "Auth parse failed."
            -- putStrLn e
            sPutStrLn "Bye"
        Right key -> do
            pm <- readMVar mpm
            case Map.lookup key pm of
                Nothing -> do
                    -- putStrLn "Auth failed."
                    sPutStrLn "BadKey"
                    preAuth h mpm
                Just phoneInfo -> do
                    sPutStrLn "Hello"
                    inHeader h phoneInfo

inputBlank :: Protocol a ProtocolError ()
inputBlank = do
    line <- inputLine
    if BS.length line == 0
        then pure ()
        else die NonemptyLine

upload :: Protocol a ProtocolError ()
upload = do
    line <- inputLine
    case () of
        _ | line == "Upload" -> do
            env <- uploadHeader
            body <- input (headerLength env)
            undefined
        _ | line == "End" -> pure ()
        _ -> die UnexpectedInput

uploadHeader :: Protocol a ProtocolError MessageEnvelope
uploadHeader = do
    line <- inputLine
    undefined
    
inHeader :: Handle -> PhoneInfo -> IO ()
inHeader h phone = do
    let sPutStrLn l = C8.hPutStrLn h l *> echoOut l
    let sGetLine = C8.hGetLine h >>= \l -> echoIn l *> pure (l <> "\n")

    mhds <- headerLoop h

    case mhds of
        Nothing -> do
            sPutStrLn "Bye"

        Just hds -> do
            let q x = if x == "me" then phoneNumber phone else x
            let pickLength = \case HeaderLength x -> Just x ; _ -> Nothing
            let pickTime   = \case HeaderTime x   -> Just x ; _ -> Nothing
            let pickFrom   = \case HeaderFrom p   -> Just p ; _ -> Nothing
            let pickTo     = \case HeaderTo p     -> Just p ; _ -> Nothing
            let pickId     = \case HeaderId x     -> Just x ; _ -> Nothing
            let get f      = listToMaybe . catMaybes . map f $ hds

            case (,,,,) <$> get pickLength <*> get pickTime <*> get pickFrom <*> get pickTo <*> get pickId of
                Nothing -> do
                    -- putStrLn "Missing headers."
                    sPutStrLn "Bye"
                Just (len, time, from, to, id) -> do
                    let fhd = MessageEnvelope from to time id len
                    sPutStrLn "GoAhead"
                    b <- C8.hGet h len
                    echoIn b

                    if BS.length b < len
                        then do
                            -- putStrLn "data ended prematurely."
                            sPutStrLn "Bye"
                        else do
                            -- putStrLn "Parsed a full message! :D"
                            sGetLine
                            sPutStrLn "Ok"
                            inHeader h phone

headerLoop :: Handle -> IO (Maybe [Header])
headerLoop h = do
    let sPutStrLn l = C8.hPutStrLn h l *> echoOut l
    let sGetLine = C8.hGetLine h >>= \l -> echoIn l *> pure (l <> "\n")

    line <- sGetLine

    case parseOnly (header <* endOfInput) line of
        Left e -> do
            -- putStrLn "Header parse failed"
            _ <- headerLoop h
            pure Nothing
        Right e -> case e of
            -- Reached a blank line
            Nothing -> do
                -- putStrLn "Reached blank."
                pure (Just [])
            Just hd -> do
                -- putStrLn "Parsed header."
                hds <- headerLoop h
                pure $ (:) <$> pure hd <*> hds

now :: T.UTCTime
now = unsafePerformIO T.getCurrentTime
{-# NOINLINE now #-}

echoIn :: BS.ByteString -> IO ()
echoIn b = C8.putStr "< " *> C8.putStrLn b

echoOut :: BS.ByteString -> IO ()
echoOut b = C8.putStr "> " *> C8.putStrLn b
