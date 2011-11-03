module Electrotech where

import Param
import Resistance
import Methods
import IO

--rp i =0.0

iu :: Double -> Double -> [[Double]]
iu tau time = iu' 0 i0 uc0
    where iu' time' i uc | time'>time = [[time',i,uc,rp i]]
                         | otherwise  = [time',i,uc,rp i]:(iu' (time' + tau) i' uc')
            where uc' = uc - tau/(2*ck)*(i+i')
                  i' = iteration f i 1e-1
                    where f = (\x -> (tau/lk*uc+(1-0.25*tau^2/lk/ck-0.5*tau/lk*(rk+ rp i))*i)/(0.25*tau^2/lk/ck + 0.5*tau/lk*(rk + rp x) + 1))

iu' :: Double -> Double -> [[Double]]
iu' tau time = iu' 0 i0 uc0
     where iu' time' i uc | time'>time = [[time',i,uc]]
                          | otherwise  = [time',i,uc]:(iu' (time' + tau) i' uc')
             where uc' = uc - tau/(2*ck)*(i+i')
                   i' = iteration f i 1e-1
                     where f = (\x -> (tau/lk*uc+(1-0.25*tau^2/lk/ck-0.5*tau/lk*(rk))*i)/(0.25*tau^2/lk/ck + 0.5*tau/lk*(rk) + 1))

                    
test :: IO ()
test = do bracket (openFile "result\\iur.txt" WriteMode)
                  hClose
                  (\h -> do mapM_ (\x -> hPutStrLn h (foldl (\s a -> s++(show a)++"\t") "" x)) (iu 5e-6 600e-6))
          bracket (openFile "result\\iu.txt" WriteMode)
                  hClose
                  (\h -> do mapM_ (\x -> hPutStrLn h (foldl (\s a -> s++(show a)++"\t") "" x)) (iu' 5e-6 600e-6))
     