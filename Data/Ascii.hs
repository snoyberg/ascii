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
    , unsafeFromBuilder
    , toBuilder
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char (isAscii)
import Data.String (IsString (..))
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Blaze.ByteString.Builder as Blaze
import Data.Monoid (Monoid)
import Data.CaseInsensitive (FoldCase, CI, mk, original)

newtype Ascii = Ascii ByteString
    deriving (Show, Eq, Read, Ord, Data, Typeable, IsString, FoldCase, Monoid)

type CIAscii = CI Ascii

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
toCIAscii = mk

fromCIAscii :: CIAscii -> Ascii
fromCIAscii = original

toByteString :: Ascii -> ByteString
toByteString (Ascii bs) = bs

toString :: Ascii -> String
toString (Ascii bs) = S8.unpack bs

toText :: Ascii -> Text
toText (Ascii bs) = TE.decodeASCII bs

ciToByteString :: CIAscii -> ByteString
ciToByteString = toByteString . original

toAsciiBuilder :: Ascii -> AsciiBuilder
toAsciiBuilder (Ascii bs) = AsciiBuilder $ Blaze.fromByteString bs

fromAsciiBuilder :: AsciiBuilder -> Ascii
fromAsciiBuilder (AsciiBuilder b) = Ascii $ Blaze.toByteString b

newtype AsciiBuilder = AsciiBuilder (Blaze.Builder)
    deriving Monoid

unsafeFromBuilder :: Blaze.Builder -> AsciiBuilder
unsafeFromBuilder = AsciiBuilder

toBuilder :: AsciiBuilder -> Blaze.Builder
toBuilder (AsciiBuilder b) = b
