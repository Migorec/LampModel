module Temperature where

import Tables
import Param
import Methods

tz :: Double -> (Double -> Double)
--tz i 0 =  t0 (abs i)
tz i z =  tn - (tn-tw)*(z)**(m (abs i))
    where tn = t0 (abs i)
    
    
nextT :: Double -> -- E
         [Double]-> -- Tp
         [Double]-> --ntp
         [Double]-> --T
nextT e tp ntp =     


itT ::  Double -> --E
        [Double] -> --TXsi
        [Double] -> --ntp
        [Double] -> --T
itT e tXsi ntp = if err < 1e-4 
                 then tn
                 else itT e tnXsi ntp
            where tn = sweep$mx e tXsi ntp
                  tnXsi = zipWith (+) tn (map (*xsi) (zipWith (-) tn tXsi))
                  err = maximum (zipWith (\x t = abs((t - x)/t)) tXsi tn)
        
        