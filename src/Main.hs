{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Network.SMSync.Server ( server, defaultServerSettings )

main :: IO ()
main = server defaultServerSettings
