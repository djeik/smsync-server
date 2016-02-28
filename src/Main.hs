{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

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

data HandlerRequest
    = TerminateHandler
    deriving (Eq, Ord, Read, Show)

data HandlerResponse
    = HandlerOK
    deriving (Eq, Ord, Read, Show)

data HandlerStateLabel
    = PreHello
    | PreAuth
    | InHeader
    | InBody
    deriving (Eq, Ord, Read, Show)

data Header
    = HeaderFrom !BS.ByteString !(Maybe BS.ByteString)
    | HeaderTo !BS.ByteString !(Maybe BS.ByteString)
    | HeaderTime !T.UTCTime
    | HeaderId !Int
    | HeaderLength !Int
    deriving (Eq, Ord, Read, Show)

data MessageHeader a
    = MessageHeader
        { headerFrom :: !(BS.ByteString, (Maybe BS.ByteString))
        , headerTo :: !(BS.ByteString, (Maybe BS.ByteString))
        , headerTime :: !T.UTCTime
        , headerId :: !Int
        , headerLength :: !Int
        }
    deriving (Eq, Ord, Read, Show)

type HeaderMap
    = Map.Map Header BS.ByteString

data HandlerState
    = HandlerState
        { stLabel :: HandlerStateLabel
        , stHeaders :: HeaderMap
        , stBody :: BS.ByteString
        }

data PhoneInfo
    = PhoneInfo
        { phoneNumber :: !BS.ByteString
        , phoneOwner :: !BS.ByteString
        }

type PhoneMap
    = Map.Map BS.ByteString PhoneInfo

main :: IO ()
main = do
    serverSocket <- socket AF_INET Stream defaultProtocol
    bind serverSocket . SockAddrInet 7777 =<< inet_addr "0.0.0.0"
    listen serverSocket 5
    clientMap <- newMVar $
        Map.fromList
            [ ( "XXX"
              , PhoneInfo
                { phoneNumber = "5145033100"
                , phoneOwner = "Jake"
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

echoIn :: BS.ByteString -> IO ()
echoIn b = C8.putStr "< " *> C8.putStrLn b

echoOut :: BS.ByteString -> IO ()
echoOut b = C8.putStr "> " *> C8.putStrLn b

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

inHeader :: Handle -> PhoneInfo -> IO ()
inHeader h phone = do
    let sPutStrLn l = C8.hPutStrLn h l *> echoOut l
    let sGetLine = C8.hGetLine h >>= \l -> echoIn l *> pure (l <> "\n")

    -- putStrLn "Beginning header loop"

    mhds <- headerLoop h

    -- putStrLn "Completed header loop"

    case mhds of
        Nothing -> do
            sPutStrLn "Bye"

        Just hds -> do
            let q x = if x == "me" then phoneNumber phone else x
            let pickLength = \case HeaderLength x -> Just x        ; _ -> Nothing
            let pickTime   = \case HeaderTime x   -> Just x        ; _ -> Nothing
            let pickFrom   = \case HeaderFrom x y -> Just (q x, y) ; _ -> Nothing
            let pickTo     = \case HeaderTo x y   -> Just (q x, y) ; _ -> Nothing
            let pickId     = \case HeaderId x     -> Just x        ; _ -> Nothing
            let get f      = listToMaybe . catMaybes . map f $ hds

            case (,,,,) <$> get pickLength <*> get pickTime <*> get pickFrom <*> get pickTo <*> get pickId of
                Nothing -> do
                    -- putStrLn "Missing headers."
                    sPutStrLn "Bye"
                Just (len, time, from, to, id) -> do
                    let fhd = MessageHeader from to time id len
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

greeting :: Parser ()
greeting = string "SMS\n" *> pure ()

auth :: Parser BS.ByteString
auth = do
    string "Key "
    k <- takeTill (== fromIntegral (fromEnum '\n'))
    string "\n"
    pure k

fie :: Char -> Word8
fie = fromIntegral . fromEnum

header :: Parser (Maybe Header)
header = choice [from, to, time, id, len, blank] where
    from = fmap pure $ do
        string "From "
        HeaderFrom
            <$> takeTill (\x -> x == fie ' ' || x == fie '\n')
            <*> choice
                [ string " " *> fmap Just (takeTill (== fie '\n') <* string "\n")
                , string "\n" *> pure Nothing
                ]
    to = fmap pure $ do
        string "To "
        HeaderTo
            <$> takeTill (\x -> x == fie ' ' || x == fie '\n')
            <*> choice
                [ string " " *> fmap Just (takeTill (== fie '\n') <* string "\n")
                , string "\n" *> pure Nothing
                ]
    time = fmap pure $ do
        string "Time "
        _ <- takeByteString
        pure $ HeaderTime now

    id = fmap pure $ do
        string "Id "
        mn <- readMaybe . C8.unpack <$> takeTill (== fie '\n')
        string "\n"
        case mn of
            Nothing -> fail "integer message id"
            Just n -> pure $ HeaderId n

    len = fmap pure $ do
        string "Length "
        mn <- readMaybe . C8.unpack <$> takeTill (== fie '\n')
        string "\n"
        case mn of
            Nothing -> fail "integer length"
            Just n -> pure $ HeaderLength n

    blank = string "\n" *> pure Nothing

now :: T.UTCTime
now = unsafePerformIO T.getCurrentTime
{-# NOINLINE now #-}
