-- jakub wozny, q1

howManyBelowAverage :: Int -> Int -> Int -> Int
howManyBelowAverage x y z = do {
    if a < ((a+b+c)/3) then do {
        if b < ((a+b+c)/3) then 2
        else do {
            if c < ((a+b+c)/3) then 2 else 1
        }
    } else do {
        if b < ((a+b+c)/3) then do {
            if c < ((a+b+c)/3) then 2 else 1
        } else do {
            if c < ((a+b+c)/3) then 1 else 0
        }
    }
} where   a = fromIntegral x :: Float
          b = fromIntegral y :: Float
          c = fromIntegral z :: Float

howManyBelowAverage1 :: Int -> Int -> Int -> Int
howManyBelowAverage1 x y z | a < ((a+b+c)/3) && b < ((a+b+c)/3) = 2
                           | a < ((a+b+c)/3) && c < ((a+b+c)/3) = 2
                           | b < ((a+b+c)/3) && c < ((a+b+c)/3) = 2
                           | a < ((a+b+c)/3) = 1
                           | b < ((a+b+c)/3) = 1
                           | c < ((a+b+c)/3) = 1
                           | otherwise = 0
                            where a = fromIntegral x :: Float
                                  b = fromIntegral y :: Float
                                  c = fromIntegral z :: Float


                                  
q1Test :: Int -> Int -> Int -> Bool 
q1Test x y z = howManyBelowAverage x y z == howManyBelowAverage1 x y z

do1qTest :: Bool
do1qTest = q1Test 1 2 3