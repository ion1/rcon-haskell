{-
© 2012 Johan Kiviniemi <devel@johan.kiviniemi.name>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Rcon.Tests.Serialize
( tests
) where

import           Network.Rcon.Serialize

import           Control.Applicative
import qualified Data.ByteString                      as BS
import           Data.Serialize
import           Data.Word
import           Test.Framework                       (Test, testGroup)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.QuickCheck

tests :: Test
tests = testGroup "Network.Rcon.Tests.Serialize"
                  [ testProperty "encodeDecodeQ" (againstQ prop_encodeDecode)
                  , testProperty "encodeDecodeR" (againstR prop_encodeDecode)
                  , testProperty "shortQ" (againstQ prop_short)
                  , testProperty "shortR" (againstR prop_short)
                  , testProperty "longQ" (againstQ prop_long)
                  , testProperty "longR" (againstR prop_long)
                  ]

againstQ :: Testable a => (QueryPacket -> a) -> (QueryPacket -> a)
againstQ = id

againstR :: Testable a => (ResponsePacket -> a) -> (ResponsePacket -> a)
againstR = id

-- decode . encode ≡ Right

prop_encodeDecode :: (Serialize a, Eq a) => a -> Bool
prop_encodeDecode packet = (decode . encode) packet == Right packet

-- Removing the last byte results in parse failure.

prop_short :: Serialize a => a -> Bool
prop_short = corrupt_prop BS.init

-- Adding an extra byte results in parse failure.

prop_long :: Serialize a => a -> Word8 -> Bool
prop_long packet extra = corrupt_prop (`BS.snoc` extra) packet

corrupt_prop :: Serialize a
             => (BS.ByteString -> BS.ByteString) -> a -> Bool
corrupt_prop corrupt packet =
  isLeft $ (decode . corrupt . encode) packet `asTypeOf` Right packet

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

instance Arbitrary QueryPacket where
  arbitrary = oneof [ AuthQ        <$> arbitrary <*> nonNullBS
                    , ExecCommandQ <$> arbitrary <*> nonNullBS
                    ]

  shrink (AuthQ id_ pass)
     =  AuthQ <$> shrink id_ <*> pure pass
    <|> AuthQ <$> pure id_   <*> shrinkNonNullBS pass

  shrink (ExecCommandQ id_ cmd)
     =  ExecCommandQ <$> shrink id_ <*> pure cmd
    <|> ExecCommandQ <$> pure id_   <*> shrinkNonNullBS cmd
    <|> pure (AuthQ id_ cmd)

instance Arbitrary ResponsePacket where
  arbitrary = oneof [ AuthR <$> arbitrary <*> nonNullBS
                    , DataR <$> arbitrary <*> nonNullBS
                    ]

  shrink (AuthR id_ data_)
     =  AuthR <$> shrink id_ <*> pure data_
    <|> AuthR <$> pure id_   <*> shrinkNonNullBS data_

  shrink (DataR id_ data_)
     =  DataR <$> shrink id_ <*> pure data_
    <|> DataR <$> pure id_   <*> shrinkNonNullBS data_
    <|> pure (AuthR id_ data_)

nonNullBS :: Gen BS.ByteString
nonNullBS = BS.pack <$> listOf (choose (1, maxBound))

shrinkNonNullBS :: BS.ByteString -> [BS.ByteString]
shrinkNonNullBS = map (BS.pack . map unPositive)
                . shrink
                . map Positive . BS.unpack
  where
    unPositive (Positive a) = a

-- vim:set et sw=2 sts=2:
