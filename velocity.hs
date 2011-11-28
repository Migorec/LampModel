module Velocity where

import Param
import Tables


fst3 (x,_,_) = x 

{-vel :: [Double] -> --t
       Double   -> --P
       [Double] -> --ntp
       [Double]-}
vel ts p ntp = zipWith3 (\z n s -> -r/z/n*s) zGrid ntn (reverse$fst3$foldl f ([0],0,0) (tail$zip3 ntp ntn zGrid))
    where ntn = map (\t -> nt p t) ts
          f (s:ss,ds,zp) (ntp,ntn,zn) = (((ds+dsn)/2*(zn-zp):s:ss),dsn,zn)
            where dsn = (ntn-ntp)/tau*zn