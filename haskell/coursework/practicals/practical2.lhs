> import Geography 
> import Maze

Exercise 1

> opposite :: Direction -> Direction
> opposite N = S
> opposite E = W 
> opposite S = N
> opposite W = E

> move :: Direction -> Place -> Place 
> move N (i,j) = (i,j+1)
> move E (i,j) = (i+1,j)
> move S (i,j) = (i,j-1)
> move W (i,j) = (i-1,j)

Exersize 2

% > drawMaze :: Maze -> IO()

> ew_row :: Maze -> Int -> Int -> String 
> ew_row m r 0      | hasWall m (r,0) W = "|"
>                   | otherwise         = " " 
> ew_row m r n      | hasWall m (r,n) E = ew_row m r (n-1) ++ "|"
>                   | otherwise         = ew_row m r (n-1) ++ " "
