-- This is demo solution for question 3 prepared by Jakub Wozny (2014114)
-- TODO:
-- - Optimize relativeDirection function (iii) if possible
-- - replace if statement somehow (also relativeDirection)
-- - test sanitizeDirections (iv)
-- - optimize sanitizeDirections (iv) if possible

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
--this function checks if route given leads to the position given, returns Boolean respectivelly
checkLoop :: [Direction] -> Bool
checkLoop route = if (0,0) == followDirections (0,0) route then True else False

-- this gets rid of loops that go through the starting point, for example: 
-- sanitizeDirection 5 [North,East,South,West,West] will get rid of the loop and will return [West]
-- it does not sanitize inner loops
sanitizeDirection :: Int -> [Direction] -> [Direction]
sanitizeDirection x dirs = if checkLoop (take x dirs)
                                then drop x dirs --return sanitized Direction array (without loop)
                                else sanitizeDirection (x-1) dirs --call it with decreased x

sanitizeDirections :: [Direction] -> [Direction]
sanitizeDirections dirs = if ((length sanitizedDirs) == 0) then [] -- when the sanitized array is empty, the starting point was the same as finish point, therefore there is no need to add the directions
                            else do{
                                if dirs == sanitizedDirs -- when dirs is equal to sanitized dirs, route was not going through the starting point, therefore first element is not part of a loop and should be kept
                                    then head dirs : sanitizeDirections (tail dirs)
                                    else sanitizeDirections sanitizedDirs
                            }
                            where sanitizedDirs = sanitizeDirection (length dirs) dirs :: [Direction]
