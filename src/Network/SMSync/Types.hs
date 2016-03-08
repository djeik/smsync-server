{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.SMSync.Types where

import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import Control.Monad.Free
import qualified Data.Map as Map
import qualified Data.Time as T

-- | Represents a message to be uploaded to a device.
data MessageUpload
    = MessageUpload
        { msgEnvelope :: !MessageEnvelope
        -- ^ The headers included in the upload.
        , msgBody :: !BS.ByteString
        -- ^ The body of the uploaded message.
        }

-- | Metadata about a message to be uploaded.
data MessageEnvelope
    = MessageEnvelope
        { headerFrom :: !PhoneInfo
        -- ^ The sender of the message.
        -- (@From@ header)

        , headerTo :: !PhoneInfo
        -- ^ The recipient of the message.
        -- (@To@ header)

        , headerTime :: !T.UTCTime
        -- ^ The delivery time of the message.
        -- (@Time@ header)

        , headerId :: !Int
        -- ^ A device-specific identifier for the message.
        -- (@Id@ header)

        , headerLength :: !Int
        -- ^ The length of the message body.
        -- (@Length@ header)

        , headerOriginalId :: !(Maybe Int)
        -- ^ The original identifier of the message.
        -- (Optional, @OriginalId@ header)

        , headerOrigin :: !MessageOrigin
        -- ^ The origin of the message.
        -- Not associated with any headers.
        }
    deriving (Eq, Ord, Read, Show)

-- | Indicates what kind of device a message originated from.
-- This is important when considering message identifiers. See the /Message
-- identifiers/ section in the SMSP documentation.
data MessageOrigin
    = PhoneOrigin
    -- ^ The message originated from an SMS-enabled device.
    | SyntheticOrigin
    -- ^ The message originated from a non-SMS-enabled device.
    deriving (Eq, Ord, Read, Show)

-- | The different kinds of headers that can be used in message uploads.
data Header
    = HeaderFrom !PhoneInfo
    | HeaderTo !PhoneInfo
    | HeaderTime !T.UTCTime
    | HeaderId !Int
    | HeaderLength !Int
    deriving (Eq, Ord, Read, Show)

type PhoneMap
    = Map.Map AuthKey PhoneInfo

data Mode
    = UploadMode
    | DownloadMode
    | ConfirmMode
    deriving (Eq, Ord, Read, Show)

type AuthKey
    = BS.ByteString

data PhoneInfo
    = PhoneInfo
        { phoneNumber :: !BS.ByteString
        , phoneOwner :: !(Maybe BS.ByteString)
        }
    deriving (Eq, Ord, Read, Show)

data ProtocolError
    = NonemptyLine
    | UnexpectedInput
    | ParseError

data ProtocolF p e next
    -- IO
    = Output BS.ByteString next
    -- ^ Emits a line of text.
    | InputLine (BS.ByteString -> next)
    -- ^ Retrieves a line of text.
    | Input Int (BS.ByteString -> next)
    -- ^ Retrieves a given number of bytes of text.
    | Parse (P.Parser p) BS.ByteString (p -> next)
    -- ^ Parses a line of text.
    -- PhoneMap interaction
    | GetPhoneMap (PhoneMap -> next)
    -- ^ Atomically read the PhoneMap without acquiring locks.
    | TakePhoneMap (PhoneMap -> next)
    -- ^ Takes the PhoneMap for exclusive use.
    | PutPhoneMap PhoneMap next
    -- ^ Stores the PhoneMap. The PhoneMap must have been taken out a priori.
    -- Exceptions
    | Die e
    -- ^ Throws an exception.
    deriving (Functor)

type Protocol p e = Free (ProtocolF p e)

output :: BS.ByteString -> Protocol p e ()
output b = liftF $ Output b ()

inputLine :: Protocol p e BS.ByteString
inputLine = liftF $ InputLine id

input :: Int -> Protocol p e BS.ByteString
input n = liftF $ Input n id

parse :: P.Parser p -> BS.ByteString -> Protocol p e p
parse p b = liftF $ Parse p b id

getPhoneMap :: Protocol p e PhoneMap
getPhoneMap = liftF $ GetPhoneMap id

takePhoneMap :: Protocol p e PhoneMap
takePhoneMap = liftF $ TakePhoneMap id

putPhoneMap :: PhoneMap -> Protocol p e ()
putPhoneMap pm = liftF $ PutPhoneMap pm ()

die :: e -> Protocol p e a
die e = liftF $ Die e

catch :: Protocol p e a -> (e -> Protocol p e' a) -> Protocol p e' a
catch p h = iterM f p where
    -- f :: ProtocolF p e (Protocol p e' a) -> Protocol p e' a
    f m = case m of
        Die e -> h e
        -- TODO: there has to be a better way ...
        Output x y -> Free $ Output x y
        InputLine x -> Free $ InputLine x
        Input x y -> Free $ Input x y
        Parse x y z -> Free $ Parse x y z
        GetPhoneMap x -> Free $ GetPhoneMap x
        TakePhoneMap x -> Free $ TakePhoneMap x
        PutPhoneMap x y -> Free $ PutPhoneMap x y
