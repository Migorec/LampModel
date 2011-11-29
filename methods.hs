module Methods (trapezeIntegrate,
                tableTrapezeIntegrate,
                dihotomy,
                iteration,
                sweep) 
where


trapezeIntegrate f a b n = ((sum $ map f [a + h, a + 2*h .. b - h]) + t) * h
    where
        t = (f a + f b)/2
        h = (b - a) / n

        
-- хорошо бы переписать с хвостовой рекурсией
tableTrapezeIntegrate :: [(Double,Double)] -> Double
tableTrapezeIntegrate [(x1,y1),(x2,y2)] = (y1+y2)/2*(x2-x1)
tabletrapezeIntegrate ((x1,y1):(x2,y2):xys) =(y1+y2)/2*(x2-x1) + tableTrapezeIntegrate ((x2,y2):xys)
        
dihotomy f a b eps | abs(a-b) < eps = x
                   | f x == 0 = x
                   | f a * f x < 0 = dihotomy f a x eps
                   | otherwise = dihotomy f x b eps
    where x = (a+b)/2
    
iteration f x eps | abs (x-xn){-/abs( xn)-} < eps = xn
                  | otherwise = iteration f xn eps
    where xn = f x
    
    
sweep' :: Double -> Double -> [(Double,Double,Double,Double)] -> [Double]
sweep' xsi eta [(a,b,d,f),(an,bn,_,fn)]= [y,yn]
    where xsi' = d/(b - xsi*a)
          eta' = (eta * a + f)/(b- xsi*a)
          y = xsi'*yn+eta'
          yn = (an*eta'+fn)/(bn-an*xsi')
sweep' xsi eta ((a,b,d,f):ls) = y:ys
    where xsi' = d/(b - xsi*a)
          eta' = (eta * a + f)/(b- xsi*a)
          ys = sweep' xsi' eta' ls
          yn = head ys
          y = xsi'*yn+eta'

    
sweep :: [(Double,Double,Double,Double)] -> [Double]
sweep ((_,b,d,f):ls) = y:ys
    where xsi = d/b
          eta = f/b
          y=xsi*y2+eta
          y2 = head ys
          ys = sweep' xsi eta ls