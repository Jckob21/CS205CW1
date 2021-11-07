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

-- logic behind does not make any sense yet
howManyBelowAverage2 :: Int -> Int -> Int -> Int
howManyBelowAverage2 x y z = do {
    if a < avg then do {
        if b < avg then 2
        else do {
            if c < avg then 2 else 1
        }
    } else do {
        if b < avg then do {
            if c < avg then 2 else 1
        } else do {
            if c < avg then 1 else 0
        }
    }
} where a = fromIntegral x :: Float
        b = fromIntegral y :: Float
        c = fromIntegral z :: Float
        avg = (a + b + c) / 3 :: Float