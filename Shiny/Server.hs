module Shiny.Server where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)

import Shiny.Focus
import Shiny.Hardware
import Shiny.Hardware.Dummy

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
  hw <- mkDummyHardware 10
  disp <- readDisplay hw
  let newDisp = unfocus $ apply (fmap (+100)) $ range 0 3 disp
  updateDisplay hw newDisp
  newDisp2 <- readDisplay hw
  print newDisp2
  clientHandler socket
