{-# LANGUAGE OverloadedStrings #-}

module Network.SMSync.Parser where

import Network.SMSync.Types

import Data.Attoparsec.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Time as T
import Data.Word
import Text.Read ( readMaybe )

greeting :: Parser ()
greeting = string "SMS\n" *> pure ()

-- | Consumes everything up to and including a newline. The returned
-- 'ByteString' does /not/ include the newline.
restOfLine :: Parser BS.ByteString
restOfLine = takeTill (== fie '\n') <* newline

auth :: Parser AuthKey
auth = string "Key " *> restOfLine

mode :: Parser Mode
mode = string "Mode " *> choice (map f modes) where
    f (k, m) = string k *> pure m
    modes =
        [ ( "Upload\n", UploadMode )
        , ( "Download\n", DownloadMode )
        , ( "Confirm\n", ConfirmMode )
        ]

headers :: Parser [Header]
headers = do
    mh <- header
    case mh of
        Just h -> (:) <$> pure h <*> headers
        Nothing -> pure []

header :: Parser (Maybe Header)
header = choice [from, to, time, id, len, blank] where
    from = fmap pure $ do
        string "From "
        fmap HeaderFrom $ PhoneInfo
            <$> takeTill (\x -> x == fie ' ' || x == fie '\n')
            <*> choice
                [ string " " *> fmap Just restOfLine
                , string "\n" *> pure Nothing
                ]
    to = fmap pure $ do
        string "To "
        fmap HeaderTo $ PhoneInfo
            <$> takeTill (\x -> x == fie ' ' || x == fie '\n')
            <*> choice
                [ string " " *> fmap Just restOfLine
                , string "\n" *> pure Nothing
                ]
    time
        = fmap pure
        $ fmap HeaderTime
        $ parseIso8601 . C8.unpack =<< (string "Time " *> restOfLine)

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

iso8601 :: String
iso8601 = T.iso8601DateFormat $ Just "%H:%M:%S%Q%z"

parseIso8601 :: Monad m => String -> m T.UTCTime
parseIso8601 = T.parseTimeM False T.defaultTimeLocale iso8601

newline :: Parser Word8
newline = word8 (fie '\n')

fie :: Char -> Word8
fie = fromIntegral . fromEnum
