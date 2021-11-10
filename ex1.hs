howManyBelowAverage :: Int -> Int -> Int -> Int

howManyBelowAverage x y z = if checkX x y z > 0
                            then do{
                                if checkY x y z > 0
                                    then do{
                                        if checkZ x y z > 0
                                            then 3
                                            else 2
                                    }
                                    else do{
                                        if checkZ x y z > 0
                                            then 2
                                            else 1
                                    }
                            }
                            else do{
                                if checkY x y z > 0
                                    then do{
                                        if checkZ x y z > 0
                                            then 2
                                            else 1
                                    }
                                    else do{
                                        if checkZ x y z > 0
                                            then 1
                                            else 0
                                    }
                            }

                           
howManyBelowAverage1 :: Int -> Int -> Int -> Int
howManyBelowAverage1 x y z | checkX x y z > 0 && checkY x y z > 0 && checkZ x y z > 0   = 3 
howManyBelowAverage1 x y z | checkX x y z > 0 && checkY x y z > 0 && checkZ x y z <= 0  = 2 
howManyBelowAverage1 x y z | checkX x y z > 0 && checkY x y z <= 0 && checkZ x y z > 0   = 2
howManyBelowAverage1 x y z | checkX x y z > 0 && checkY x y z <= 0 && checkZ x y z <= 0   = 1
howManyBelowAverage1 x y z | checkX x y z <= 0 && checkY x y z > 0 && checkZ x y z > 0   = 2 
howManyBelowAverage1 x y z | checkX x y z <= 0 && checkY x y z > 0 && checkZ x y z <= 0   = 1
howManyBelowAverage1 x y z | checkX x y z > 0 && checkY x y z <= 0 && checkZ x y z > 0   = 1
howManyBelowAverage1 x y z | checkX x y z > 0 && checkY x y z <= 0 && checkZ x y z <= 0   = 0


computeAverage :: Int -> Int -> Int -> Float

computeAverage x y z = (a + b + c) / 3
                        where a = fromIntegral x :: Float
                              b = fromIntegral z :: Float
                              c = fromIntegral y :: Float

checkX :: Int -> Int -> Int -> Int
checkX x y z  | a < computeAverage x y z     = x
              | a >= computeAverage x y z     = 0
                where a = fromIntegral x :: Float
checkY :: Int -> Int -> Int -> Int
checkY x y z  | b < computeAverage x y z     = y
              | b >= computeAverage x y z     = 0
                where b = fromIntegral y :: Float

checkZ :: Int -> Int -> Int -> Int
checkZ x y z  | c < computeAverage x y z     = z
              | c >= computeAverage x y z     = 0
                where c = fromIntegral z :: Float



q1Test :: Int -> Int -> Int -> Bool 

q1Test x y z = howManyBelowAverage x y z == howManyBelowAverage1 x y z

do1qTest :: Bool
do1qTest = q1Test 1 2 3
