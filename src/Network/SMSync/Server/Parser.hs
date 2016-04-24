{-# LANGUAGE OverloadedStrings #-}

module Network.SMSync.Server.Parser where

import Network.SMSync.Types ( AuthToken )
import Network.SMSync.Misc ( fie )

import Data.Attoparsec.ByteString

greeting :: Parser ()
greeting = string "SMS\n" *> pure ()

auth :: Parser AuthToken
auth = do
    string "Key "
    k <- takeTill (== fie '\n')
    string "\n"
    pure k

-- | Parses a line from a header block.
-- header :: Parser (Maybe Header)
header = undefined -- choice [from, to, time, id, len, blank] where
