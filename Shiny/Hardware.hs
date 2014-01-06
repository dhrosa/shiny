-- | Hardware interfaces for LED displays
module Shiny.Hardware (Hardware(..)) where

import Shiny.Shiny (Display)

-- | Interface to the LED hardware
data Hardware = Hardware {
  readDisplay   :: IO (Display),
  updateDisplay :: Display -> IO (),
  displaySize   :: IO (Int),
  resetDisplay  :: IO ()
  }