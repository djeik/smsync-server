module Network.SMSync.Server where

data ServerSettings
    = ServerSettings
        { serverHost :: String
        , serverPort :: Int
        }
    deriving (Eq, Ord, Read, Show)


