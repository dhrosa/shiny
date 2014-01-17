module Shiny.Hardware.Serial where

import Shiny.Shiny
import Shiny.Hardware

import System.Hardware.Serialport

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import Data.IORef

mkSerialHardware :: FilePath -> Int -> IO (Hardware)
mkSerialHardware path size = do
  port <- openSerial path (defaultSerialSettings {commSpeed = CS115200})
  -- store a local copy of the display
  ref <- newIORef (emptyDisplay size)
  let toBytes (RGB r g b) = B.pack [r, g, b]
    
      readDisplay = readIORef ref
                  
      writeDisplay disp = do
        writeIORef ref disp
        sent <- send port . B.concat . map toBytes $ disp
        putStrLn ("Sent " ++ show sent ++ " bytes")
        return ()
    
      displaySize = return size
    
      resetDisplay = do
        setDTR port True
        threadDelay (500*1000)
        setDTR port False
        threadDelay (500*1000)
        return ()
  
  return $ Hardware readDisplay writeDisplay displaySize resetDisplay