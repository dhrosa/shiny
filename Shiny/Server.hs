module Shiny.Server where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

echo :: Handle -> IO()
echo client = do
  line <- hGetLine client
  hPutStrLn client line
  echo client

clientHandler :: Socket -> IO()
clientHandler sock = do
  (client, _, _) <- accept sock
  hSetBuffering client NoBuffering
  forkIO $ echo client
  clientHandler sock

main = do
  socket <- listenOn $ PortNumber 3000
  putStrLn $ "Server started."
  clientHandler socket
