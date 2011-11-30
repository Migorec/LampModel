module Temperature where

import Tables
import Param
import Methods
import Pressure
import RadTransfer

tz :: Double -> (Double -> Double)
--tz i 0 =  t0 (abs i)
tz i z =  tn - (tn-tw)*(z)**(m (abs i))
    where tn = t0 (abs i)
    
    
{-nextT :: Double -> -- E
         [Double]-> -- Tp
         [Double]-> --ntp
         [Double]-> --T
nextT e tp ntp = itT e tp ntp
-}

ad :: [Double] -> [(Double,Double)] --коэффициенты a и d для распределения температуры в начальный момент времени
ad t = a t z2Grid p2Wave pr
    where pr = tableP (zip zGrid t)
          a [t1,t2] [z1] [pw] pr = []
          a (tp:tn:tm:ts) (z2p:z2m:z2s) (p2p:p2m:p2s) pr = 
            (tau/hx/r/r*z2p*p2p*(lambda pr ((tp+tn)/2)),
             tau/hx/r/r*z2m*p2m*(lambda pr ((tm+tn)/2))):(a (tn:tm:ts) (z2m:z2s) (p2p:p2s) pr)
          
{-          
itT ::  Double -> --E
        [Double] -> --TXsi
        [Double] -> --ntp
        [Double] -> --T
itT e tXsi ntp = if err < 1e-4 
                 then tn
                 else itT e tnXsi ntp
            where tn = sweep$mx e tnXsi ntp
                  tnXsi = zipWith (+) tn (map (*xsi) (zipWith (-) tn tXsi))
                  err = maximum (zipWith (\x t = abs((t - x)/t)) tXsi tn)
                  
        
mx e tnXsi ntp = (0,-1,1,0):(mTail e pr vn ntp z2Grid tnXsi pWave p2Wave qn)
    where pr = tableP (zip zGrid tnXsi)
          vn = vel tnXsi pr ntp
          df = divF (\z -> interp0olate z (zip zGrid tnXsi)) pr
          qn = map (\t d -> sigma pr t * e * e - d ) (zip tnXsi df)
     

mTail _ [tx] [nt] [z2n] = [(0,-1,0,-2000)]

mTail e pr (vp:vn:vs) (z2p:z2m:z2s) (tp:tn:tm:ts) (pp:pn:pm:ps) (p2p:p2m:p2s) (qp:qn:qm:qs) =
            
            where vN = (z2m*z2m - z2p*z2p)/2
                  m = (cpt pr tn)/2/hx/r*vN*tau*pn
                  t2p = (tp+tn)/2
                  t2m = (tm+tn)/2
                  aN = m*(abs vn + vn) + tau/hx/r/r*z2p*(lambda pr t2p)*p2p
                  dN = m*(abs vn - vn) + tau/hx/r/r*z2m*(lambda pr t2m)*p2m
                  bN = aN + dN + (cpt pr tn)*vN
                  fN = (cpt pr tn)*vN*tn + qn*tau*vN + )_
                  
-}