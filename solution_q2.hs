-- q2 jakub wozny

pizzaPricing :: Float -> Int -> Int -> Float
pizzaPricing d t s = truncateDigits ((area * 0.002 + area * 0.001 * (fromIntegral t) + (fromIntegral s) * 0.5) * 1.5) 2
    where area =  pi * (d/2)^2 :: Float

truncateDigits :: Float -> Int -> Float
truncateDigits num digits = (fromIntegral (floor (num * t))) / t
    where t = 10^digits