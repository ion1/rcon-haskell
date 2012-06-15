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

module Network.Rcon.Parse
( parseQueryPacket
, parseResponsePacket
) where

-- https://developer.valvesoftware.com/wiki/Source_RCON_Protocol

import           Network.Rcon.Types

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec
import qualified Data.Attoparsec     as AP
import           Data.Bits           (shiftL, (.|.))
import qualified Data.ByteString     as BS
import           Data.Word

parseQueryPacket :: BS.ByteString -> Either String QueryPacket
parseQueryPacket = parseOnly (queryPacket <* endOfInput)

parseResponsePacket :: BS.ByteString -> Either String ResponsePacket
parseResponsePacket = parseOnly (responsePacket <* endOfInput)

queryPacket :: Parser QueryPacket
queryPacket =
  do id_   <- word32le
     type_ <- word32le
     case type_ of
       3 -> authQ id_
       2 -> execCommandQ id_
       _ -> fail ("queryPacket: unknown type " ++ show type_)

responsePacket :: Parser ResponsePacket
responsePacket =
  do id_   <- word32le
     type_ <- word32le
     case type_ of
       2 -> authR id_
       0 -> dataR id_
       _ -> fail ("responsePacket: unknown type " ++ show type_)

authQ :: Word32 -> Parser QueryPacket
authQ id_ = AuthQ id_ <$> (byteString0 <* emptyByteString0)

execCommandQ :: Word32 -> Parser QueryPacket
execCommandQ id_ = ExecCommandQ id_ <$> (byteString0 <* emptyByteString0)

authR :: Word32 -> Parser ResponsePacket
authR id_ = AuthR id_ <$> (byteString0 <* emptyByteString0)

dataR :: Word32 -> Parser ResponsePacket
dataR id_ = DataR id_ <$> (byteString0 <* emptyByteString0)

word32le :: Parser Word32
word32le  =  do [a, b, c, d] <- replicateM 4 (fromIntegral <$> anyWord8)
                return  $  (a `shiftL` 0x00)
                       .|. (b `shiftL` 0x08)
                       .|. (c `shiftL` 0x10)
                       .|. (d `shiftL` 0x18)
         <?> "word32le"

byteString0 :: Parser BS.ByteString
byteString0  =  AP.takeWhile (/= 0) <* word8 0
            <?> "0-terminated ByteString"

emptyByteString0 :: Parser ()
emptyByteString0  =  () <$ word8 0
                 <?> "empty 0-terminated ByteString"

-- vim:set et sw=2 sts=2:
