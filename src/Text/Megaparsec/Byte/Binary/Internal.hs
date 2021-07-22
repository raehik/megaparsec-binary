{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Helpers for parsing Word8 lists as "machine" integers to Haskell types.
module Text.Megaparsec.Byte.Binary.Internal where

import           Data.Bits
import           Data.Word
import           Data.Foldable          ( foldl' )

-- | Convert a 'Word8' list to a little-endian numeric type.
--
-- The target type must fit the resulting value (caller responsibility).
bytesBE :: (Bits a, Num a) => [Word8] -> a
bytesBE = foldl' go 0
  where go acc byte = (acc `shiftL` 8) .|. fromIntegral byte

-- | Parse a 'Word8' list as a big-endian number.
--
-- The target type must fit the resulting value (caller responsibility).
bytesLE :: (Bits a, Num a) => [Word8] -> a
bytesLE = fst . (foldl' go (0, 0))
  where
    go (acc, index) byte = (acc+x, index+1)
      where
        x = fromIntegral byte `shiftL` (8 * index)

-- | Return the number of bytes in the argument.
--
-- Performs ceiling divison, so byte-unaligned types (bitsize not a multiple of
-- 8) should work, but further usage is not tested.
finiteByteSize :: forall a. (FiniteBits a) => Int
finiteByteSize = finiteBitSize @a undefined `ceilDiv` 8

ceilDiv :: Integral a => a -> a -> a
ceilDiv x y = (x+y-1) `div` y
