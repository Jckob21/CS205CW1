-- GROUP NUMBER: 28
-- MEMBERS:
-- Jakub Wozny (2014114)
-- Przemek Rabczak (2013650)
-- Kacper Rozwadowski (2024343)
-- Please note: Our solution is based on coursework document from 09/11/2021

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------  QUESTION 1  ------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------

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

howManyBelowAverage2 :: Int -> Int -> Int -> Int
howManyBelowAverage2 x y z | a < ((a+b+c)/3) && b < ((a+b+c)/3) = 2
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
q1Test x y z = howManyBelowAverage x y z == howManyBelowAverage2 x y z

doq1Test :: Bool
doq1Test = q1Test 2014114 2013650 2024343

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

-------------------------------------------------------------------------------
-------------------------------------  I  -------------------------------------
-------------------------------------------------------------------------------

-- complete - checks whether a binary tree is a complete tree
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Bool - dependly if the tree is a complete then true, otherwise it will produce false

complete :: Btree a -> Bool
complete (Binary l x r) = if complete l && complete r && abs (depth l - depth r) <= 1 && isUnary l
                            then not (perfect r || isUnary r)
                            else not (isUnary l && perfect r)
                            
complete (Leaf x) = True
complete (Unary l x) = lastLeaf l

-- isUnary - checks whether a node is unary
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Bool - if the current node is Unary outputs True, otherwise returns false.

isUnary :: Btree a -> Bool
isUnary (Unary l x) = True
isUnary (Binary l x r) = False
isUnary (Leaf x) = True

-- lastLeaf - checks whether a current node is a last leaf
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Bool - Outputs True only if the current node (a) is leaf, if it is binary or unary then it is false.

lastLeaf :: Btree a -> Bool
lastLeaf (Unary l x) = False
lastLeaf (Binary l x r) = False
lastLeaf (Leaf x) = True

-- perfect - checks whether a binary tree is a perfect tree
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Bool - dependly if the tree is a perfect then true, otherwise it will produce false

perfect :: Btree a -> Bool
perfect (Binary l x r) = perfect l && perfect r
perfect (Unary l x) = False
perfect (Leaf x) = True

-- depth - computes depth for a binary tree or the depth from selected depth
-- Parameters:
-- Btree a - type of current node, it can be a binary, unary or leaf
-- Output:
-- Int - depth of a tree/node

depth :: Btree a -> Int
depth (Leaf x) = 0
depth (Binary left x right) = 1 + min (depth left) (depth right)
depth (Unary left x) = 1 + depth left

-------------------------------------------------------------------------------
-------------------------------------  II  ------------------------------------
-------------------------------------------------------------------------------

-- lookupInSearchTree - Looks up a element which is situated on specific node with first component (k) in the tree
-- Parameters:
-- Integer - (k) First element contained on the current node
-- Btree (Integer, a) - type of current node, it can be a binary, unary or leaf, which contains two elements on the node
-- Output:
-- Maybe a - If there exist some element (v) with first component (k) then It return: Just some element, otherwise if there is no such an element it returns Nothing

lookupInSearchTree :: Integer -> Btree (Integer, a) -> Maybe a

lookupInSearchTree k (Leaf(x, v)) | k == x     = Just y
                                   | otherwise  = Nothing
                                    where y = v


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

-- insertInSearchTree - Inserts another node with two values (Integer,a) to the tree
-- Parameters:
-- Integer - (k) First element to the new node
-- a - second element to the new node
-- Btree (Integer, a) - type of current node, it can be a binary, unary or leaf, which contains two elements on the node
-- Output:
-- Btree (Integer, a) - Outputs all values of the third input of tree type with one copy of new node


insertInSearchTree :: Integer -> a -> Btree (Integer,a) -> Btree (Integer,a)

insertInSearchTree k chr (Leaf(x,y)) = if k > x 
                                        then Unary oldNodeToNew (k, chr)
                                        else Unary newleft (x,y)  
                                        where newleft = Leaf(k,chr)
                                              oldNodeToNew = Leaf(x,y)

insertInSearchTree k chr (Binary l (x,y) r) = if k > x
                                               then Binary l (x,y) (insertInSearchTree k chr r)
                                               else Binary (insertInSearchTree k chr l) (x,y) r

insertInSearchTree k chr (Unary l (x,y)) =  if k > x
                                             then Binary l (x,y) (Leaf(k,chr))
                                             else Unary (insertInSearchTree k chr l) (x,y)
