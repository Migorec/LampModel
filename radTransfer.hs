module RadTransfer where

import Tables
import Methods
import Param 

import Temperature
import Pressure

divF :: (Double -> Double)      -- T(z) 
        -> Double               -- P 
        -> [(Double,Double)]    -- difF (z)
divF t p = zip zGrid (f (replicate (length zGrid) 0.0) (map (\(nu, un) -> (nu,map (\(z,u)-> (c*(knu p (t z) nu)*((up (t z) nu)-u))) un))   
                                                  (map  (\x -> (x,divFnu t p x)) freqTable)))
    where f ss [l] = ss
          f ss ((nu1,l1):(nu2,l2):ls) = f   (zipWith (+) (zipWith (\(u1) (u2) -> (u1+u2)/2*(nu2-nu1)) l1 l2) ss) ((nu2,l2):ls)

          
{-divFnu :: (Double -> Double)      -- T(z) 
        -> Double               -- P 
        -> Double               -- nu
        -> [(Double,Double)]    -- difF (z)-}
-- переименовать на самом деле это [(z,u)]        
divFnu t p nu = zip zGrid (sweep$mx t p nu)
             
mx t p nu = (0,-k0,m0,-p0):(mTail t p nu z2Grid zGrid)
    where h1 = (head$tail zGrid) - head zGrid
          zd2 = (head z2Grid)--(head zGrid + (head$tail zGrid))/2
          kd2 = knu p (t zd2) nu
          k0 = zd2*(1/(3*r*r*h1*kd2) +h1/8*kd2)
          m0 = zd2*(h1/8*kd2-1/(3*r*r*h1*kd2))
          p0 = h1/4*kd2*zd2*(up (t zd2)nu)
          
mTail t p nu [zd2] [z,zn] = [(kN,-mN,0,-pN)]
    where hn = zn-z
          kd2 = knu p (t zd2) nu
          kn = knu p (t zn) nu
          mN = mr/2/r+zd2/(3*r*r*kd2*hn)+kn*hn/4+hn*kd2*zd2/8
          kN = -zd2/(3*r*r*kd2*hn)+hn*kd2*zd2/8
          upn = up (t zn) nu
          upd2 = up (t zd2) nu
          pN = hn/4*(kn*upn+kd2*upd2*zd2)
mTail t p nu (zn2:zp2:z2s)(zm:zn:zp:zs) = (aN,bN,dN,fN):(mTail t p nu (zp2:z2s) (zn:zp:zs))
    where hn = zn-zm
          hp = zp-zn
          kn2 = knu p (t zn2) nu
          kp2 = knu p (t zp2) nu
          kn = knu p (t zn) nu
          upn = up (t zn) nu
          vN = (zp2*zp2-zn2*zn2)/2
          aN = zn2/hn/kn2
          dN = zp2/hp/kp2
          bN = aN + dN +3*r*r*kn*vN
          fN = 3*r*r*kn*upn*vN
          
    
k=1.38e-23 
mr = 0.786

up :: Double -> Double ->  Double
up t nu = 8*pi*hp*nu*nu*nu/(c*c*c*(exp (hp*nu/k/t)-1))
