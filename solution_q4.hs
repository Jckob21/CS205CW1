-- q4 jakub wozny
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
