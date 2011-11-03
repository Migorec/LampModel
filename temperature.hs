module Temperature where

import Tables
import Param

tz :: Double -> (Double -> Double)
--tz i 0 =  t0 (abs i)
tz i z =  tn - (tn-tw)*(z)**(m (abs i))
    where tn = t0 (abs i)