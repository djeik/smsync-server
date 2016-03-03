module Network.SMSync.Types where

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

-- | The different kinds of headers that can be used in message uploads.
data Header
    = HeaderFrom !PhoneInfo
    | HeaderTo !PhoneInfo
    | HeaderTime !T.UTCTime
    | HeaderId !Int
    | HeaderLength !Int
    deriving (Eq, Ord, Read, Show)

type PhoneMap
    = Map.Map BS.ByteString PhoneInfo

data PhoneInfo
    = PhoneInfo
        { phoneNumber :: !BS.ByteString
        , phoneOwner :: !(Maybe BS.ByteString)
        }

