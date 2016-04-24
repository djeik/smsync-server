module Network.SMSync.Misc where

-- | Convert 'Enum' values into 'Num' values.
--
-- The specialization to @Char -> Word8@ is particularly useful.
fie :: (Enum a, Num b) => a -> b
fie = fromIntegral . fromEnum
