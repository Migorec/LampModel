module RadTransfer where

import Tables
import Methods
import Param 

{-divF :: (Double -> Double)      -- T(z) 
        -> Double               -- P 
        -> [(Double,Double)]    -- difF (z)-}
divF t p = f (replicate (length zGrid) 0.0)  (map  (\x -> (x,divFnu t p x)) freqTable)
    where f ss [l] = ss
          f ss ((nup,_):(nun,un):ls) = zipWith (\s (z,n) -> s + c*(knu p (t z) nun)*((up (t z) nun)- n)*(nun - nup) ) ss un
          
{-divFnu :: (Double -> Double)      -- T(z) 
        -> Double               -- P 
        -> Double               -- nu
        -> [(Double,Double)]    -- difF (z)-}
-- переименовать на самом деле это [(z,u)]        
divFnu t p nu = zip zGrid (sweep (mx:(mTail t p nu zGrid)))
    where mx = (zd2*(1/(3*r*r*h1*kd2))+h1/8*kd2,-zd2*(h1/8*kd2-1/(3*r*r*h1*kd2)),0,-h1/4*kd2*zd2*(up (t zd2)nu))
          h1 = (head$tail zGrid) - head zGrid
          zd2 = (head zGrid + (head$tail zGrid))/2
          kd2 = knu p (t zd2) nu
    
mTail t p nu [z,zn] = [(0,-kN,mN,-pN)]
    where zd2 = (z+zn)/2
          hn = zn-z
          kd2 = knu p (t zd2) nu
          kn = knu p (t zn) nu
          mN = mr/2/r+zd2/(3*r*r*kd2*hn)+kn*hn/4+hn*kd2*zd2/8
          kN = -zd2/(3*r*r*kd2*hn)+hn*kd2*zd2/8
          upn = up (t zn) nu
          upd2 = up (t zd2) nu
          pN = hn/4*(kn*upn+kd2*upd2*zd2)
mTail t p nu (zm:zn:zp:zs) = (aN,bN,dN,fN):(mTail t p nu (zn:zs))
    where zn2 = (zm+zn)/2
          zp2 = (zn+zp)/2
          hn = zn-zm
          hp = zp-zn
          kn2 = knu p (t zn2) nu
          kp2 = knu p (t zp2) nu
          kn = knu p (t zn) nu
          upn = up (t zn) nu
          vN = (zp2*zp2-zn2*zn2)/2
          aN = zn2/hn/kn2
          dN = zp2/hp/kp2
          bN = aN + dN +3*r*r*r*kn*vN
          fN = 3*r*r*r*kn*upn*vN
          
    
k=7242 
mr = 0.786

up :: Double -> Double ->  Double
up t nu = 8*pi*hp*nu*nu*nu/(c*c*c*(exp (hp*nu/k/t)-1))
