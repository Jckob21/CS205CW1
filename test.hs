import Data.List (sort)
-- napisac info o grupie 

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------  QUESTION 1  ------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------


-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------  QUESTION 2  ------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

pizzaPricing :: Float -> Int -> Int -> Float
pizzaPricing d t s = truncateDigits ((area * 0.002 + area * 0.001 * (fromIntegral t) + (fromIntegral s) * 0.5) * 1.5) 2
    where area =  pi * (d/2)^2 :: Float

truncateDigits :: Float -> Int -> Float
truncateDigits num digits = (fromIntegral (floor (num * t))) / t
    where t = 10^digits

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------  QUESTION 3  ------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-------------------------------------  I  -------------------------------------
-------------------------------------------------------------------------------

-- data for question 3
data Direction = North | West | South | East deriving (Show,Eq)

-- followDirection - transforms position based on direction given
-- Parameters:
-- (Int,Int) - position
-- Direction - direction given
-- Output:
-- (Int,Int) - new position affected by the direction
followDirection :: (Int,Int) -> Direction -> (Int,Int)
followDirection (x,y) West = (x-1,y)
followDirection (x,y) East = (x+1,y)
followDirection (x,y) South = (x,y-1)
followDirection (x,y) North = (x,y+1)

-------------------------------------------------------------------------------
------------------------------------  II  -------------------------------------
-------------------------------------------------------------------------------

-- followDirection - transforms position based on given array of directions
-- Parameters:
-- (Int,Int) - position
-- [Direction] - directions determinating the path
-- Output:
-- (Int,Int) - new position affected by the array of directions
followDirections :: (Int,Int) -> [Direction] -> (Int,Int)
followDirections pos [] = pos
followDirections pos (dir:dirs) = followDirections (followDirection pos dir) dirs

-------------------------------------------------------------------------------
------------------------------------  III  ------------------------------------
-------------------------------------------------------------------------------

-- data for III)
data RelativeDirection = GoForward | GoBack | GoLeft | GoRight
    deriving Show

-- relativizeDirections - Given initial direction and a path, transforms the cardinal directions to a relative stepwise path
-- Parameters:
-- Direction - initial cardinal direction
-- [Direction] - cardinal directions determinating the path
-- Output:
-- [RelativeDirection] - relative stepwise path based on given cardinal directions
relativizeDirections :: Direction -> [Direction] -> [RelativeDirection]
relativizeDirections pos [] = []
relativizeDirections pos (dir:dirs) = (relativeDirection pos dir) : (relativizeDirections dir dirs)

-- relativeDirection - Taking two directions defines RelativeDirection
-- Parameters:
-- Direction - initial cardinal direction
-- Direction - second cardinal direction that determines relative direction
-- Output:
-- RelativeDirection - relative direction of the two cardinal directions
relativeDirection :: Direction -> Direction -> RelativeDirection
relativeDirection North East = GoRight
relativeDirection North West = GoLeft
relativeDirection North South = GoBack
relativeDirection East South = GoRight
relativeDirection East North = GoLeft
relativeDirection East West = GoBack
relativeDirection South West = GoRight
relativeDirection South East = GoLeft
relativeDirection South North = GoBack
relativeDirection West North = GoRight
relativeDirection West South = GoLeft
relativeDirection West East = GoBack
relativeDirection _ _  = GoForward

-------------------------------------------------------------------------------
------------------------------------  IV  -------------------------------------
-------------------------------------------------------------------------------

-- checkLoop - checks if the path given leads to starting point
-- Parameters:
-- [Direction] - array of directions forming the path
-- Output:
-- Bool - true, if path ends on the same position when it started, false otherwise
checkLoop :: [Direction] -> Bool
checkLoop route = if (0,0) == followDirections (0,0) route then True else False

-- sanitizeDirection - removes parts of the path (sets of Directions) that end on the same position
-- *** Examples ***
-- 1) sanitizeDirection 5 [North,East,South,West,West] RESULT: [West], removed loop occuring at indexes 0-3
-- 2) sanitizeDirection 5 [North,North,East,South,West] RESULT: [North,North,East,South,West] not removed the loop as it starts on index 1
-- Parameters:
-- Int - finish index of the part currently checked
-- [Direction] - array of directions forming the path
-- Output:
-- [Direction] - array of directions without parts of the path that end on the same position
sanitizeDirection :: Int -> [Direction] -> [Direction]
sanitizeDirection x dirs = if checkLoop (take x dirs)
                                then drop x dirs --return sanitized Direction array (without the loop)
                                else sanitizeDirection (x-1) dirs -- if there was no loop, call it with decreased x

-- sanitizeDirections - sanitizes the path given completely from loops
-- Parameters:
-- [Direction] - array of directions forming the path
-- Output:
-- [Direction] - sanitized array of directions without any loops
sanitizeDirections :: [Direction] -> [Direction]
sanitizeDirections dirs = if ((length sanitizedDirs) == 0) then [] -- when the sanitized array is empty, the starting point was the same as finish point, therefore there is no need to add the directions
                            else do{
                                if dirs == sanitizedDirs 
                                    then head dirs : sanitizeDirections (tail dirs) -- when dirs is equal to sanitized dirs, route was not going through the starting point, therefore first element is not part of a loop and should be kept
                                    else sanitizeDirections sanitizedDirs -- if sanitizeDirection removed a loop, there is no element that was checked last time so it calls sanitizeDirections on the new array
                            }
                            where sanitizedDirs = sanitizeDirection (length dirs) dirs :: [Direction]

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------  QUESTION 4  ------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

--data and types for maze question
data Orientation = H | V deriving (Show,Eq)
type Wall = (Int, Int, Orientation) -- we consider cartesian coordinate system starting at the bottom left
type Maze = ((Int,Int),[Wall])

-- example maze taken from coursework document
exampleMaze :: Maze
exampleMaze = ((4,4), hWalls ++ vWalls)
    where   vWalls = map (\ (i,j) -> (i,j,V)) [ -- vertical walls, iterated by columns
                (0,0),(0,1),(0,2),(0,3),
                (1,1),(1,2),
                (2,1),(2,2),
                (3,2),(3,3),
                (4,0),(4,1),(4,2)]
            hWalls = map (\ (i,j) -> (i,j,H)) [ -- horizontal walls, iterated by rows
                (0,0),(1,0),(2,0),(3,0),
                (0,1),(2,1),
                (2,2),
                (0,4),(1,4),(2,4),(3,4)]

-- our examples of mazes to test algorithm
exampleMazeOur1 :: Maze -- not solvable
exampleMazeOur1 = ((3,3), hWalls ++ vWalls)
    where   vWalls = map (\ (i,j) -> (i,j,V)) [ -- vertical walls, iterated by columns
                (0,0),(0,1),(0,2),
                (1,1),(1,2),
                (3,0),(3,1)]
            hWalls = map (\ (i,j) -> (i,j,H)) [ -- horizontal walls, iterated by rows
                (0,0),(1,0),(2,0),
                (1,1),(2,1),
                (1,2),
                (0,3),(1,3),(2,3)]

exampleMazeOur2 :: Maze -- solvable
exampleMazeOur2 = ((3,3), hWalls ++ vWalls)
    where   vWalls = map (\ (i,j) -> (i,j,V)) [ -- vertical walls, iterated by columns
                (0,0),(0,1),(0,2),
                (1,1),(1,2),
                (3,0),(3,1)]
            hWalls = map (\ (i,j) -> (i,j,H)) [ -- horizontal walls, iterated by rows
                (0,0),(1,0),(2,0),
                (1,1),
                (1,2),
                (0,3),(1,3),(2,3)]

exampleMazeOur3 :: Maze -- non-solvable, starting position closed
exampleMazeOur3 = ((3,3), hWalls ++ vWalls)
    where   vWalls = map (\ (i,j) -> (i,j,V)) [ -- vertical walls, iterated by columns
                (0,0),(0,1),(0,2),
                (1,0),(1,1),(1,2),
                (3,0),(3,1)]
            hWalls = map (\ (i,j) -> (i,j,H)) [ -- horizontal walls, iterated by rows
                (0,0),(1,0),(2,0),
                (0,1),(1,1),
                (1,2),
                (0,3),(1,3),(2,3)]

-------------------------------------------------------------------------------
---------------------------------  solution  ----------------------------------
-------------------------------------------------------------------------------

-- getDirectionsOut - solves maze with approach "always turning in the most leftward direction possible"
-- Parameters:
-- Maze - maze to be solved
-- Output:
-- Maybe [Direction] - If maze is possible to be solved, Just xs is returned (where xs is an array of directions
--                          that indicate the path), Nothing otherwise
getDirectionsOut :: Maze -> Maybe [Direction]
getDirectionsOut maze = if((isStartingPositionOpen maze) && (checkIfPossibleToSolve maze (0,0) North []))
                            then Just (goDirectionsOut maze (0,0) North)
                            else Nothing

-- checkIfPossibleToSolve - checks if maze is possible to solve (has a way out)
-- WARNING: IT DOES NOT CHECK WHETHER FIRST POSSITION IS OPEN
-- Parameters:
-- Maze - maze
-- (Int,Int) - current position algorithm is pointing to
-- Direction - last direction algorithm used (orientation)
-- [Direction] - array of directions indicating how the algorithm got to the current point (by default [])
-- Output:
-- Bool - True if you can solve the maze, false otherwise
checkIfPossibleToSolve :: Maze -> (Int,Int) -> Direction -> [Direction] -> Bool
checkIfPossibleToSolve maze (x,y) dir route = if (x >= fst (getMazeSize maze))
                                            then True
                                            else do {
                                                if(length route > 2 && checkLoop (take (length route - 1) route) && route!!0 == route!!(length route - 1))
                                                    then False
                                                    else checkIfPossibleToSolve maze (followDirection (x,y) newDirection) newDirection (route ++ [newDirection])
                                            }
                                            where newDirection = goDirectionOut maze (x,y) dir :: Direction

-- isStartingPositionOpen - checks if starting position in maze has any way to go 
-- Parameters:
-- Maze - maze
-- Output:
-- Bool - True if you can go forward or right, false otherwise
isStartingPositionOpen :: Maze -> Bool
isStartingPositionOpen maze = if(isGoForwardPossible maze (0,0) North || isGoRightPossible maze (0,0) North)
                                then True
                                else False

-- goDirectionsOut - given a position and a direction algorithm recursively finds a path to the exit  
-- Parameters:
-- Maze - maze to be solved
-- (Int,Int) - current position algorithm is pointing to
-- Direction - last direction algorithm used (orientation)
-- Output:
-- [Direction] - array of directions indicating the path to solve the maze
goDirectionsOut :: Maze -> (Int,Int) -> Direction -> [Direction]
goDirectionsOut maze (x,y) dir = if(x >= fst (getMazeSize maze)) -- check if position is out of maze (solved) assuming exit on the right side because of cw1 doc
                                    then []
                                    else newDirection : goDirectionsOut maze (followDirection (x,y) newDirection) newDirection
                                    where newDirection = goDirectionOut maze (x,y) dir :: Direction

-- goDirectionsOut - given the position in the maze, finds next Direction according to
--                      "always turning in the most leftward direction possible" approach    
-- Parameters:
-- Maze - maze to be solved
-- (Int,Int) - position where the algorithm is pointing to
-- Direction - last direction algorithm used (orientation)
-- Output:
-- Direction - next Direction of the path
goDirectionOut :: Maze -> (Int,Int) -> Direction -> Direction
goDirectionOut maze (x,y) dir = if(isGoLeftPossible maze (x,y) dir)
                                    then cardinalDirection dir GoLeft
                                    else do {
                                        if(isGoForwardPossible maze (x,y) dir)
                                            then cardinalDirection dir GoForward
                                            else do {
                                                if(isGoRightPossible maze (x,y) dir)
                                                    then cardinalDirection dir GoRight
                                                    else cardinalDirection dir GoBack
                                            }
                                    }

-- isGoLeftPossible - based on given position and orientation, checks if you can go left in given maze
-- Parameters:
-- Maze - maze to be solved
-- (Int,Int) - current position
-- Direction - orientation
-- Output:
-- Bool - True if you can go left from that position, false otherwise
isGoLeftPossible :: Maze -> (Int,Int) -> Direction -> Bool
isGoLeftPossible maze (x,y) North = if (x,y,V) `elem` (getWalls maze) then False else True
isGoLeftPossible maze (x,y) East = if (x,y+1,H) `elem` (getWalls maze) then False else True
isGoLeftPossible maze (x,y) South = if (x+1,y,V) `elem` (getWalls maze) then False else True
isGoLeftPossible maze (x,y) West = if (x,y,H) `elem` (getWalls maze) then False else True

-- isGoForwardPossible - based on given position and orientation, checks if you can go forward in given maze
-- Parameters:
-- Maze - maze to be solved
-- (Int,Int) - current position
-- Direction - orientation
-- Output:
-- Bool - True if you can go forward from that position, false otherwise
isGoForwardPossible :: Maze -> (Int,Int) -> Direction -> Bool
isGoForwardPossible maze (x,y) North = if (x,y+1,H) `elem` (getWalls maze) then False else True
isGoForwardPossible maze (x,y) East = if (x+1,y,V) `elem` (getWalls maze) then False else True
isGoForwardPossible maze (x,y) South = if (x,y,H) `elem` (getWalls maze) then False else True
isGoForwardPossible maze (x,y) West = if (x,y,V) `elem` (getWalls maze) then False else True

-- isGoLeftPossible - based on given position and orientation, checks if you can go right in given maze
-- Parameters:
-- Maze - maze to be solved
-- (Int,Int) - current position
-- Direction - orientation
-- Output:
-- Bool - True if you can go right from that position, false otherwise
isGoRightPossible :: Maze -> (Int,Int) -> Direction -> Bool
isGoRightPossible maze (x,y) North = if (x+1,y,V) `elem` (getWalls maze) then False else True
isGoRightPossible maze (x,y) East = if (x,y,H) `elem` (getWalls maze) then False else True
isGoRightPossible maze (x,y) South = if (x,y,V) `elem` (getWalls maze) then False else True
isGoRightPossible maze (x,y) West = if (x,y+1,H) `elem` (getWalls maze) then False else True

-- getMazeSize - gets size of the maze given
-- Parameters:
-- Maze - the maze to get size of
-- Output:
-- (Int,Int) - size of the Maze, where first Int represents width, second Int represents Height
getMazeSize :: Maze -> (Int,Int)
getMazeSize maze = fst maze

-- getWalls - gets walls of the maze given
-- Parameters:
-- Maze - the maze to get walls of
-- Output:
-- [Wall] - the walls of the Maze
getWalls :: Maze -> [Wall]
getWalls maze = snd maze

-- cardinalDirection - gets direction based on initial direction and orientation
-- Parameters:
-- Direction - initial Direction
-- RelativeDirection - relative direction defining change of the orientation
-- Output:
-- Direction - new direction after changing the orientation
cardinalDirection :: Direction -> RelativeDirection -> Direction
cardinalDirection North GoLeft = West
cardinalDirection North GoBack = South
cardinalDirection North GoRight = East
cardinalDirection East GoLeft = North
cardinalDirection East GoBack = West
cardinalDirection East GoRight = South
cardinalDirection South GoLeft = East
cardinalDirection South GoBack = North
cardinalDirection South GoRight = West
cardinalDirection West GoLeft = South
cardinalDirection West GoBack = East
cardinalDirection West GoRight = North
cardinalDirection a GoForward = a

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------  QUESTION 5  ------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------


data Btree a = Leaf a | Unary (Btree a) a | Binary (Btree a) a (Btree a)
            deriving Show

ex1 = Unary(Unary(Unary(Unary(Unary(Unary(Unary (Leaf 0) 1)2)3)4)5)6)7
ex2 = Binary (Binary (Leaf 0) 1 (Leaf 2)) 3 (Unary (Leaf 4) 5)
ex3 = Binary (Unary (Leaf 1) 2) 3 (Unary (Leaf 4) 5)
ex4 = Binary (Unary (Leaf 1) 2) 3 (Binary (Leaf 4) 5 (Leaf 10))
ex5 = Binary (Binary (Leaf (0,"a"))(1,"z")(Leaf (2,"x")))(3,"y")(Binary (Leaf (4,"b"))(5,"c")(Leaf (6,"d")))
ex6 = Leaf 10
ex7 = Leaf (10, "a")

-------------------------------------------------------------------------------
-------------------------------------  I  -------------------------------------
-------------------------------------------------------------------------------

diff a b = abs (a-b)

complete :: Btree a -> Bool

complete (Binary l x r) = if complete l && complete r && diff (depth l) (depth r) <= 1 && isUnary l
                            then not (perfect r || isUnary r)
                            else not (isUnary l && perfect r)
                            
complete (Leaf x) = True
complete (Unary l x) = lastLeaf l

isUnary :: Btree a -> Bool
isUnary (Unary l x) = True
isUnary (Binary l x r) = False
isUnary (Leaf x) = True

lastLeaf :: Btree a -> Bool
lastLeaf (Unary l x) = False
lastLeaf (Binary l x r) = False
lastLeaf (Leaf x) = True

perfect :: Btree a -> Bool

perfect (Binary l x r) = perfect l && perfect r
perfect (Unary l x) = False
perfect (Leaf x) = True


depth :: Btree a -> Int
depth (Leaf x) = 0
depth (Binary left x right) = 1 + min (depth left) (depth right)
depth (Unary left x) = 1 + depth left

-------------------------------------------------------------------------------
-------------------------------------  II  ------------------------------------
-------------------------------------------------------------------------------


-- value t is called "search tree" if the root of every subtree of t is either a leaf or a unary or a binary
--t = Btree (Int, a)

-- All values contained in l have first components <= x
-- Unary l (x, v)

-- All values contained in l have first components <= x, and those contained in r have first components >= x
-- Binary l (x,v) r 


lookupInSearchTree :: Integer -> Btree (Integer, a) -> Maybe a

lookupInSearchTree k (Leaf(x, v)) | k == x     = Just y
                                   | otherwise  = Nothing
                                    where y = v

--lookupInSearchTree k (Leaf(x, _)) = Nothing 


lookupInSearchTree k (Binary l (x,v) r) | k == x       = Just result
                                         | k < x        = lookupInSearchTree k l
                                         | k > x        = lookupInSearchTree k r
                                          where result = v

lookupInSearchTree k (Unary l (x,v)) | k == x      = Just solution
                                      | k < x       = lookupInSearchTree k l
                                       where solution = v

-------------------------------------------------------------------------------
------------------------------------  III  ------------------------------------
-------------------------------------------------------------------------------

insertInSearchTree :: Integer -> a -> Btree (Integer,a) -> Btree (Integer,a)

insertInSearchTree k chr (Leaf(x,y)) = Unary newleft (x,y)
                                       where newleft = Leaf(k,chr)

insertInSearchTree k chr (Binary l (x,y) r) = insertInSearchTree k chr l

insertInSearchTree k chr (Unary l (x,y)) =  Binary l (x,y) (Leaf(k,chr))





-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------- TESTY -------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
--tests are named : testAqBC, where A is the test number, B is the question number and C is the part of the question(only for Q3 and Q5)
-- range of A: 1-3 (for Q5 it is 1-6 in A part of the question)
---------------------------------------------------------------Q2:

--test1
test1q1 :: IO()
test1q1 = if(pizzaPricing 30.0 3 3 == 7.55)
    then putStrLn "positive"
    else putStrLn "negative"

--test2
test2q1 :: IO()
test2q1 = if(pizzaPricing 10.0 10 0 == 1.41)
    then putStrLn "positive"
    else putStrLn "negative"

--test3
test3q1 :: IO()
test3q1 = if(pizzaPricing 0 5 20 == 15.0)
    then putStrLn "positive"
    else putStrLn "negative"
---------------------------------------------------------------Q3:

--test1A
test1q3A :: IO()
test1q3A = if(followDirection (53,13) South == (53,12))
    then putStrLn "positive"
    else putStrLn "negative"

--test2A
test2q3A :: IO()
test2q3A = if(followDirection (followDirection (followDirection(224,8) East) West) North == (224,9))
    then putStrLn "positive"
    else putStrLn "negative"

--test3A
test3q3A :: IO()
test3q3A = if(followDirection (followDirection (10,14) East) North == (11,15))
    then putStrLn "positive"
    else putStrLn "negative"

--test1B
test1q3B :: IO()
test1q3B = if(followDirections (10,0) [North,North,North,North,North,North] == (10,6))
    then putStrLn "positive"
    else putStrLn "negative"

--test2B
test2q3B :: IO()
test2q3B = if(followDirections (0,0) [North,South,North,South,West,East,West,East] == (0,0))
    then putStrLn "positive"
    else putStrLn "negative"

--test3B
test3q3B :: IO()
test3q3B = if(followDirections (-10,-10) [East,East,North,East,West,West] == (-9,-9))
    then putStrLn "positive"
    else putStrLn "negative"

--test1C
--test1q3C :: IO()
--test1q3C = if(relativizeDirections West [East,West,West,West,West,North] == [GoBack,GoBack,GoForward,GoForward,GoForward,GoRight])
--    then putStrLn "positive"
--    else putStrLn "negative"

--test2C
--test2q3C :: IO()
--test2q3C = if(relativizeDirections East [North,North,West,West,East,West] == [GoLeft,GoForward,GoLeft,GoForward,GoBack,GoBack])
--    then putStrLn "positive"
--    else putStrLn "negative"

--test3C
--test3q3C :: IO()
--test3q3C = if(relativizeDirections South [East,South,North,West,West,West] == [GoLeft,GoRight,GoBack,GoLeft,GoForward,GoForward])
--    then putStrLn "positive"
--    else putStrLn "negative"

--test1D
test1q3D :: IO()
test1q3D = if(sanitizeDirections [North,West,West,South,West,East,South,North,North] == [North,West,West])
    then putStrLn "positive"
    else putStrLn "negative"

--test2D
test2q3D :: IO()
test2q3D = if(sanitizeDirections [East,North,East,South,East,East,West,South,North] == [East,North,East,South,East])
    then putStrLn "positive"
    else putStrLn "negative"

--test3D
test3q3D :: IO()
test3q3D = if(sanitizeDirections [South,South,West,West,East,North,South,South,West] == [South,South,West,South,West])
    then putStrLn "positive"
    else putStrLn "negative"


---------------------------------------------------------------Q5:

--test1A
test1q5A :: IO()
test1q5A = if(complete ex2 == True)
    then putStrLn "positive"
    else putStrLn "negative"

--test2A
test2q5A :: IO()
test2q5A = if(complete ex1 == False)
    then putStrLn "positive"
    else putStrLn "negative"

--test3A
test3q5A :: IO()
test3q5A = if(complete ex3 == False)
    then putStrLn "positive"
    else putStrLn "negative"

--test4A
test4q5A :: IO()
test4q5A = if(perfect ex1 == False)
    then putStrLn "positive"
    else putStrLn "negative"

--test5A
test5q5A :: IO()
test5q5A = if(perfect ex5 == True)
    then putStrLn "positive"
    else putStrLn "negative"

--test6A
test6q5A :: IO()
test6q5A = if(perfect ex4 == False)
    then putStrLn "positive"
    else putStrLn "negative"

--test1B
test1q5B :: IO()
test1q5B = if(lookupInSearchTree 50 ex5 == Nothing)
    then putStrLn "positive"
    else putStrLn "negative"

--test2B
test2q5B :: IO()
test2q5B = if(lookupInSearchTree 4 ex5 == Just "b")
    then putStrLn "positive"
    else putStrLn "negative"

--test3B
test3q5B :: IO()
test3q5B = if(lookupInSearchTree 1 ex5 == Just "z")
    then putStrLn "positive"
    else putStrLn "negative"