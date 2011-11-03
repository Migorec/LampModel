module Resistance where

import Methods
import Tables
import Pressure
import Temperature
import Param




rp i = l/(2*pi*int)
    where int = trapezeIntegrate f 0 1 20
          f = (\z -> (sigma pt (t z)) * z)
          t = tz (abs i)
          pt = p t
            
          
