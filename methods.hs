module Methods where

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