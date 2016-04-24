{-# LANGUAGE GADTs #-}

module Network.SMSync.Types where

import qualified Data.ByteString as BS
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
data UploadHeader
    = ULHeaderFrom !HeaderFrom
    | ULHeaderTo !HeaderTo
    | ULHeaderTime !HeaderTime
    | ULHeaderId !HeaderId
    | ULHeaderLength !HeaderLength
    deriving (Eq, Ord, Read, Show)

-- | A header indicating the sender of a message.
data HeaderFrom = HeaderFrom !PhoneInfo
    deriving (Eq, Ord, Read, Show)

-- | A header indicating the recipient of a message.
data HeaderTo = HeaderTo !PhoneInfo
    deriving (Eq, Ord, Read, Show)

-- | A header indicating the time of a message.
data HeaderTime = HeaderTime !T.UTCTime
    deriving (Eq, Ord, Read, Show)

-- | A header indicating a unique identifier for a message.
data HeaderId = HeaderId !ClientMessageId
    deriving (Eq, Ord, Read, Show)

-- | A header indicating the length of a message.
data HeaderLength = HeaderLength !Int
    deriving (Eq, Ord, Read, Show)

-- | A header indicating the origin of a message.
data HeaderOrigin = HeaderOrigin !MessageOrigin
    deriving (Eq, Ord, Read, Show)

-- | An auth token acts as a password for an account.
type AuthToken = BS.ByteString

type ClientMessageId = Int
type ServerMessageId = Int

-- | Maps auth tokens to their associated account's information.
type PhoneMap
    = Map.Map AuthToken PhoneInfo

-- | Information about a phone.
data PhoneInfo
    = PhoneInfo
        { phoneNumber :: !BS.ByteString
        , phoneOwner :: !(Maybe BS.ByteString)
        }
    deriving (Eq, Ord, Read, Show)
