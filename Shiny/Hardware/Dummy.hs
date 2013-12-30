module Shiny.Hardware.Dummy (mkDummyHardware) where

import Shiny.Shiny
import Shiny.Hardware
import Data.IORef

mkDummyHardware :: Int -> IO(Hardware)
mkDummyHardware size = do
  ref <- newIORef (emptyDisplay size)
  return (Hardware (readIORef ref) (writeIORef ref))