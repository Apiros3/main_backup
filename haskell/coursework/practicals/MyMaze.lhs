
> module MyMaze (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography

> data Maze = AMaze Size [Place] [Place] [Place] [Place]

> makeMaze :: Size -> [Wall] -> Maze
> makeMaze (x,y) walls = 
>   let     bwN = [((i,y-1), N) | i <- [0..x-1]]
>           bwS = [((i,0),   S) | i <- [0..x-1]]
>           bwE = [((x-1,j),   E) | j <- [0..y-1]]
>           bwW = [((0,j), W) | j <- [0..y-1]]
>           bbN = filter (\(x,y) -> y == N) walls 
>           bbS = filter (\(x,y) -> y == S) walls 
>           bbE = filter (\(x,y) -> y == E) walls 
>           bbW = filter (\(x,y) -> y == W) walls 
>           wallN = map fst (bwN ++ bbN ++ map reflect (bwS ++ bbS))
>           wallS = map fst (bwS ++ bbS ++ map reflect (bwN ++ bbN))
>           wallE = map fst (bwE ++ bbE ++ map reflect (bwW ++ bbW))
>           wallW = map fst (bwW ++ bbW ++ map reflect (bwE ++ bbE))
>  in AMaze (x,y) wallN wallS wallE wallW

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (AMaze _ wallN wallS wallE wallW) pos d   | d == N    = pos `elem` wallN
>                                                   | d == S    = pos `elem` wallS 
>                                                   | d == E    = pos `elem` wallE 
>                                                   | otherwise = pos `elem` wallW 

> sizeOf :: Maze -> Size
> sizeOf (AMaze size _ _ _ _) = size

