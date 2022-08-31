{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- TODO debug

-- | "Machine" integer parsers.
module Text.Megaparsec.Byte.Binary where

import           Text.Megaparsec.Byte.Binary.Internal

import           Text.Megaparsec
import           Data.Bits
import           Data.Word
import           Data.Maybe             ( fromMaybe )
import           Data.Proxy             ( Proxy(..) )

-- TODO debug below
import qualified Data.ByteString as BS
import           Data.Void              ( Void )

-- | Parse any byte-representable value from any 'Word8' token stream using the
--   given function.
--
-- Intended for byte-aligned types. May not work as expected with byte-unaligned
-- types (bitsize not a multiple of 8).
anyFBits
    :: forall a e s m. (MonadParsec e s m, Token s ~ Word8, FiniteBits a, Num a)
    => ([Word8] -> a) -> Maybe String -> m a
anyFBits convert mLbl = do
    let reqLen = finiteByteSize @a
        lbl    = show reqLen <> "-byte " <> fromMaybe "value" mLbl
    bytes <- takeP (Just lbl) reqLen
    let bytesList = chunkToTokens (Proxy :: Proxy s) bytes
    return (convert bytesList)

--------------------------------------------------------------------------------

anyLE
    :: forall a e s m. (MonadParsec e s m, Token s ~ Word8, FiniteBits a, Num a)
    => m a
anyLE = anyFBits bytesLE (Just "little-endian integer")

anyBE
    :: forall a e s m. (MonadParsec e s m, Token s ~ Word8, FiniteBits a, Num a)
    => m a
anyBE = anyFBits bytesBE (Just "big-endian integer")

--------------------------------------------------------------------------------

anyW16LE
    :: forall e s m. (MonadParsec e s m, Token s ~ Word8)
    => m Word16
anyW16LE = anyLE

anyW32LE
    :: forall e s m. (MonadParsec e s m, Token s ~ Word8)
    => m Word32
anyW32LE = anyLE

anyW64LE
    :: forall e s m. (MonadParsec e s m, Token s ~ Word8)
    => m Word64
anyW64LE = anyLE

anyW16BE
    :: forall e s m. (MonadParsec e s m, Token s ~ Word8)
    => m Word16
anyW16BE = anyBE

anyW32BE
    :: forall e s m. (MonadParsec e s m, Token s ~ Word8)
    => m Word32
anyW32BE = anyBE

anyW64BE
    :: forall e s m. (MonadParsec e s m, Token s ~ Word8)
    => m Word64
anyW64BE = anyBE

--------------------------------------------------------------------------------

parseTest' :: Show a => Parsec Void BS.ByteString a -> BS.ByteString -> IO ()
parseTest' = parseTest

test
    :: (FiniteBits a, Num a, Show a)
    => ([Word8] -> a) -> BS.ByteString -> IO ()
test f str = parseTest' (anyFBits f Nothing) str

test'
    :: forall a. (FiniteBits a, Num a, Show a)
    => BS.ByteString -> IO ()
test' = parseTest' (anyLE @a)

test'' :: BS.ByteString -> IO ()
test'' = parseTest' anyW64BE
