module Server where

data ServerSettings
    = ServerSettings
        { serverHost :: String
        , serverPort :: Int
        }
