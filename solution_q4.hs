-- q4 jakub wozny

--functions taken from q3 (will be useful)

-----------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------
------------------------------  DUPLICATION FROM Q3, TO BE REMOVED IN FINAL SUBMISSION  -------------------------------
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
-------------------------------------  END OF DUPLICATION FROM Q3, TO BE REMOVED  -------------------------------------
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

-------------------------------------------------------------------------------
---------------------------------  solution  ----------------------------------
-------------------------------------------------------------------------------

--getDirectionsOut :: Maze -> Maybe [Direction]
--getDirectionsOut maze = 




getWalls :: Maze -> [Wall]
getWalls maze = snd maze
