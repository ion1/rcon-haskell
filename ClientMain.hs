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

{-# LANGUAGE PatternGuards #-}

module Main (main) where

import qualified Control.Exception           as Exc
import           Control.Monad
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS8
import qualified Network.Rcon.Client         as RC
import           System.Exit
import           System.IO
import           System.Posix.Env.ByteString

main :: IO ()
main =
  do args <- getArgs
     pass <- getEnv (BS8.pack "RCON_PASSWORD")

     case args of
       host : port : cmds | Just pass' <- pass ->
         runCmds host port pass' cmds

       _ -> usage >> exitFailure

runCmds :: BS.ByteString -> BS.ByteString -> BS.ByteString -> [BS.ByteString]
        -> IO ()
runCmds host port pass cmds =
  Exc.bracket (RC.connect hostS portS pass) (RC.close)
    $ \c -> forM_ cmds (RC.execCommand c >=> BS8.putStr . BS8.unlines)
  where
    hostS = BS8.unpack host
    portS = BS8.unpack port

usage :: IO ()
usage =
  hPutStrLn stderr
    "USAGE: RCON_PASSWORD=<password> rcon-client <host> <port> [<command>...]"

-- vim:set et sw=2 sts=2:
