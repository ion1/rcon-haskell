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

module Network.Rcon.Serialize
( QueryPacket (..)
, ResponsePacket (..)
) where

-- https://developer.valvesoftware.com/wiki/Source_RCON_Protocol

import           Network.Rcon.Parse
import           Network.Rcon.Types

import qualified Data.ByteString    as BS
import           Data.Serialize
import           Data.Word

instance Serialize QueryPacket where
  put (AuthQ        id_ pass) = putQOrR id_ 3 pass
  put (ExecCommandQ id_ cmd)  = putQOrR id_ 2 cmd
  get = either fail return . parseQueryPacket =<< getAll

instance Serialize ResponsePacket where
  put (AuthR id_ data_) = putQOrR id_ 2 data_
  put (DataR id_ data_) = putQOrR id_ 0 data_
  get = either fail return . parseResponsePacket =<< getAll

-- At the moment all the packets happen to follow the same format.
putQOrR :: Word32 -> Word32 -> BS.ByteString -> Put
putQOrR id_ type_ bs =
  do putWord32le id_
     putWord32le type_
     putByteString0 bs
     putByteString0 BS.empty

-- Put a null-terminated bytestring.
putByteString0 :: BS.ByteString -> Put
putByteString0 bs = putByteString (BS.takeWhile (/= 0) bs) >> putWord8 0

getAll :: Get BS.ByteString
getAll = getBytes =<< remaining

-- vim:set et sw=2 sts=2:
