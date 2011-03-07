{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Data.Ascii
    ( -- * Datatypes
      Ascii
    , CIAscii
    , AsciiBuilder
      -- * Construction
      -- ** Safe
    , fromByteString
    , fromChars
    , fromText
      -- ** Unsafe
    , unsafeFromByteString
    , unsafeFromString
    , unsafeFromText
      -- * Extraction
    , toByteString
    , toString
    , toText
      -- * Case insensitive
    , toCIAscii
    , fromCIAscii
    , ciToByteString
      -- * Builder
    , toAsciiBuilder
    , fromAsciiBuilder
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char (toLower, isAscii)
import Data.String (IsString (..))
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Blaze.ByteString.Builder as Blaze

newtype Ascii = Ascii ByteString
    deriving (Show, Eq, Read, Ord, Data, Typeable, IsString)

data CIAscii = CIAscii
    { ciOriginal :: !ByteString
    , ciLowerCase :: !ByteString
    }
    deriving (Data, Typeable)

instance Show CIAscii where
    show = show . ciOriginal
instance Read CIAscii where
    readsPrec i = map (\(x, y) -> (toCIAscii x, y)) . readsPrec i
instance Eq CIAscii where
    x == y = ciLowerCase x == ciLowerCase y
instance Ord CIAscii where
    x <= y = ciLowerCase x <= ciLowerCase y
instance IsString CIAscii where
    fromString = toCIAscii . unsafeFromString

fromByteString :: ByteString -> Maybe Ascii
fromByteString bs
    | S.all (< 128) bs = Just $ Ascii bs
    | otherwise = Nothing

-- | Renamed to avoid clash with 'fromString'
fromChars :: String -> Maybe Ascii
fromChars s
    | all isAscii s = Just $ Ascii $ S8.pack s
    | otherwise = Nothing

fromText :: Text -> Maybe Ascii
fromText t
    | T.all isAscii t = Just $ Ascii $ TE.encodeUtf8 t
    | otherwise = Nothing

unsafeFromByteString :: ByteString -> Ascii
unsafeFromByteString = Ascii

unsafeFromString :: String -> Ascii
unsafeFromString = Ascii . S8.pack

unsafeFromText :: Text -> Ascii
unsafeFromText = Ascii . TE.encodeUtf8

toCIAscii :: Ascii -> CIAscii
toCIAscii (Ascii bs) = CIAscii bs $ S8.map toLower bs

fromCIAscii :: CIAscii -> Ascii
fromCIAscii = Ascii . ciOriginal

toByteString :: Ascii -> ByteString
toByteString (Ascii bs) = bs

toString :: Ascii -> String
toString (Ascii bs) = S8.unpack bs

toText :: Ascii -> Text
toText (Ascii bs) = TE.decodeASCII bs

ciToByteString :: CIAscii -> ByteString
ciToByteString = ciOriginal

toAsciiBuilder :: Ascii -> AsciiBuilder
toAsciiBuilder (Ascii bs) = AsciiBuilder $ Blaze.fromByteString bs

fromAsciiBuilder :: AsciiBuilder -> Ascii
fromAsciiBuilder (AsciiBuilder b) = Ascii $ Blaze.toByteString b

newtype AsciiBuilder = AsciiBuilder (Blaze.Builder)
