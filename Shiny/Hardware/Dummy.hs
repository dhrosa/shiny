module Shiny.Hardware.Dummy (mkDummyHardware) where

import Shiny.Shiny
import Shiny.Hardware
import Data.IORef
import Data.Time (getCurrentTime)

mkDummyHardware :: Int -> IO (Hardware)
mkDummyHardware size = do
  ref <- newIORef (emptyDisplay size)
  return (Hardware (read ref) (writeIORef ref))
  where read ref = do
          disp <- readIORef ref
          time <- getCurrentTime
          putStrLn $ "Display at time: " ++ show time ++ "\n" ++ showDisplay disp
          return disp