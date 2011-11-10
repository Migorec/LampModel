module Methods (trapezeIntegrate,
                dihotomy,
                iteration,
                sweep) 
where

trapezeIntegrate f a b n = ((sum $ map f [a + h, a + 2*h .. b - h]) + t) * h
    where
        t = (f a + f b)/2
        h = (b - a) / n
        
dihotomy f a b eps | abs(a-b) < eps = x
                   | f x == 0 = x
                   | f a * f x < 0 = dihotomy f a x eps
                   | otherwise = dihotomy f x b eps
    where x = (a+b)/2
    
iteration f x eps | abs (x-xn){-/abs( xn)-} < eps = xn
                  | otherwise = iteration f xn eps
    where xn = f x
    
    
sweep' :: Double -> Double -> [(Double,Double,Double,Double)] -> [Double]
sweep' xsi eta [(a,b,d,f),(_,bn,dn,fn)] = [y,yn]
    where yn = (eta*bn-fn)/(dn-xsi*bn)
          y = xsi*yn + eta
sweep' xsi eta ((a,b,d,f):ls) = y:ys
            where ys = sweep' xsi' eta' ls
                  y' = head ys
                  y = xsi*y' + eta
                  xsi' = d/(b-a*xsi)
                  eta' = (a*eta+f)/(b-a*xsi)
    
sweep :: [(Double,Double,Double,Double)] -> [Double]
sweep ((a1,b1,_,f1):ls) = y1:ys
        where ys = sweep' xsi eta ls
              y2 = head ys
              y1 = b1/a1*y2-f1/a1
              xsi = b1/a1
              eta = f1/a1