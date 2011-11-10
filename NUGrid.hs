module NUGrid where

nuGrid :: [Double] -> Double -> [Double]
nuGrid xs delta = map (\x -> 1/alpha*atan (x * tan alpha)) xs
    where alpha = pi/2 - delta
    
makeGrid :: Double -> Double -> Integer -> [Double]
makeGrid a b n = [a,a+h..b]
        where h=(b-a)/(fromIntegral n)
        
makeNUGrid :: Double -> Double -> Integer -> Double ->[Double]
makeNUGrid a b n delta = nuGrid (makeGrid a b n) delta