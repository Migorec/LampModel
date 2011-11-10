module NUGrid where

nuGrid :: [Double] -> Double -> [Double]
nuGrid xs delta = map (\x -> 1/alpha*atan (x * tan alpha)) xs
    where alpha = pi/2 - delta