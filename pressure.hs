module Pressure where

import Tables
import Param
import Methods

p :: (Double -> Double) -> Double
p t = dihotomy f 3 28 1
    where f p = 2 * trapezeIntegrate (\z -> (nt p (t z)) * z) 0 1 20 - 7242 * p0 / t00 
