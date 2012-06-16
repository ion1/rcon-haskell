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

{-# LANGUAGE DeriveDataTypeable #-}

module Network.Rcon.Client
( ClientHandle
, connect
, close
, execCommand
) where

import           Network.Rcon.Serialize

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception      as Exc
import qualified Data.ByteString        as BS
import           Data.Maybe
import           Data.Serialize
import           Data.Time
import qualified Data.Traversable       as Trav
import           Data.Typeable          (Typeable)
import           Data.Word
import qualified Network.Socket         as So
import           System.IO
import           System.Timeout

newtype ClientHandle = ClientHandle (TMVar ClientMessage)

data ClientMessage = Close       (TMVar CloseResponse)
                   | ExecCommand (TMVar CommandResponse) BS.ByteString

newtype CloseResponse = CloseResponse (Either Exc.SomeException ())

newtype CommandResponse
  = CommandResponse (Either Exc.SomeException [BS.ByteString])

data ClientException = LookupFailed     String
                     | AuthFailed       String
                     | ProtocolError    String
                     | ConnectionClosed String
  deriving (Show, Typeable)
instance Exc.Exception ClientException

connect :: String -> String -> BS.ByteString -> IO ClientHandle
connect host port password =
  do addrs <- So.getAddrInfo (Just hints) (Just host) (Just port)
     addr  <- (maybe lookupFailed return . listToMaybe) addrs
     sock  <- So.socket (So.addrFamily addr)
                        (So.addrSocketType addr)
                        (So.addrProtocol addr)
     So.connect sock (So.addrAddress addr) `Exc.onException` So.sClose sock

     handle <- So.socketToHandle sock ReadWriteMode

     flip Exc.onException (hClose handle) $
       do hSetBuffering handle (BlockBuffering Nothing)

          authenticate handle password

          serverVar <- newEmptyTMVarIO
          _ <- forkIO (server (serverVar, handle, 1))

          return (ClientHandle serverVar)

  where
    hints = So.defaultHints { So.addrFlags      = [So.AI_ADDRCONFIG]
                            , So.addrSocketType = So.Stream
                            }

    lookupFailed = Exc.throwIO
                 $ LookupFailed ("Failed to resolve " ++ show host)

close :: ClientHandle -> IO ()
close (ClientHandle serverVar) =
  do clientVar <- newEmptyTMVarIO
     atomically (putTMVar serverVar (Close clientVar))
     CloseResponse res <- atomically (takeTMVar clientVar)
     either Exc.throwIO return res

execCommand :: ClientHandle -> BS.ByteString -> IO [BS.ByteString]
execCommand (ClientHandle serverVar) cmd =
  do clientVar <- newEmptyTMVarIO
     atomically (putTMVar serverVar (ExecCommand clientVar cmd))
     CommandResponse res <- atomically (takeTMVar clientVar)
     either Exc.throwIO return res

authenticate :: Handle -> BS.ByteString -> IO ()
authenticate handle password =
  do sendPacket handle (AuthQ 0 password)
     resp0 <- recvPacket handle
     resp  <- case resp0 of
       AuthR _ _ -> return resp0

       -- Some RCON servers send a junk data packet before the auth response.
       _ -> recvPacket handle

     case resp of
       AuthR 0    _ -> return ()  -- Success.
       AuthR (-1) _ -> Exc.throwIO (AuthFailed "Initial auth failed")
       _ -> Exc.throwIO (ProtocolError ("Auth response: " ++ show resp))

server :: (TMVar ClientMessage, Handle, Word32) -> IO ()
server (serverVar, handle, counter) =
  do msg <- atomically (takeTMVar serverVar)
     case msg of
       Close clientVar ->
         serverClose (serverVar, handle, counter) clientVar

       ExecCommand clientVar cmd ->
         serverExecCommand (serverVar, handle, counter) (clientVar, cmd)

serverClose :: (TMVar ClientMessage, Handle, Word32) -> TMVar CloseResponse
            -> IO ()
serverClose (serverVar, handle, _) clientVar =
  do res <- Exc.try (hClose handle)
     _   <- forkIO $ atomically (putTMVar clientVar (CloseResponse res))

     closedServer serverVar

serverExecCommand :: (TMVar ClientMessage, Handle, Word32)
                  -> (TMVar CommandResponse, BS.ByteString) -> IO ()
serverExecCommand (serverVar, handle, counter) (clientVar, cmd) =
  do -- A RCON server can send multiple response packets and there is no way to
     -- know in advance which one is the last one. :-(
     --
     -- Measure the time it takes for the server to send the first packet and
     -- wait up to five times that for the next packet until assuming there is
     -- no next packet.

     resps <- Exc.try
            $ withAutoTimeout 5 (sendPacket handle (ExecCommandQ counter cmd))
                                (recvPacket handle)

     resps' <- return $ case resps of
       Left exc -> Left (Exc.toException (exc :: Exc.SomeException))

       Right list ->
         Trav.sequence . flip map list $ \resp ->
           case resp of
             DataR id_ data_ | id_ == counter ->
               Right data_

             AuthR (-1) _ ->
               Left . Exc.toException
                 $ AuthFailed "Auth failed unexpectedly"

             _ ->
               Left . Exc.toException
                 $ ProtocolError ("ExecCommand response: " ++ show resp)

     _ <- forkIO $ atomically (putTMVar clientVar (CommandResponse resps'))

     case resps' of
       Right _ ->
         server (serverVar, handle, counter + 1)

       Left _ ->
         do hClose handle
            closedServer serverVar

closedServer :: TMVar ClientMessage -> IO ()
closedServer serverVar =
  do msg <- atomically (takeTMVar serverVar)

     _ <- case msg of
       Close clientVar ->
         forkIO . atomically . putTMVar clientVar
           $ CloseResponse (Right ())

       ExecCommand clientVar _ ->
         forkIO . atomically . putTMVar clientVar
           $ CommandResponse (Left connectionClosedExc)

     closedServer serverVar

  where
    connectionClosedExc =
      Exc.toException (ConnectionClosed "Connection closed")

sendPacket :: Handle -> QueryPacket -> IO ()
sendPacket handle packet =
  do BS.hPut handle lenEncoded
     BS.hPut handle encoded
     hFlush handle

  where
    encoded = encode packet

    lenEncoded = runPut . putWord32le . fromIntegral . BS.length $ encoded

recvPacket :: Handle -> IO ResponsePacket
recvPacket handle =
  do lenEncoded <- BS.hGet handle 4
     len <- case runGet getWord32le lenEncoded of
       Left err ->
         Exc.throwIO (ProtocolError ("Decode length: " ++ err))

       Right len ->
         return len

     encoded <- BS.hGet handle (fromIntegral len)
     case decode encoded of
       Left err ->
         Exc.throwIO (ProtocolError ("Decode response: " ++ err))

       Right packet ->
         return packet

-- | Run the first action and the action to be repeated once and measure the
-- time it took. Multiply that by a factor and keep running the action to be
-- repeated as long as each invocation finishes in that time.

withAutoTimeout :: Rational -> IO () -> IO a -> IO [a]
withAutoTimeout factor firstAction repeatAction =
  do startTime <- getCurrentTime
     firstAction

     res0 <- repeatAction
     time <- (`diffUTCTime` startTime) <$> getCurrentTime

     let timeout_µs = round (realToFrac time * factor * 1e6)

     (res0:) <$> go timeout_µs

  where
    go timeout_µs =
      do mres <- timeout timeout_µs repeatAction
         case mres of
           Just res -> (res:) <$> go timeout_µs
           Nothing  -> return []



-- vim:set et sw=2 sts=2:
