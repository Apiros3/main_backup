
> module MyMaze2 (
>   Maze, 
>   makeMaze, -- :: Size -> [Wall] -> Maze
>   hasWall,  -- :: Maze -> Place -> Direction -> Bool
>   sizeOf    -- :: Maze -> Size
> )
> where

> import Geography

> data BTree a = Leaf | Branch a (BTree a) (BTree a) 
> data Maze = AMaze Size (BTree (Int,Int)) (BTree (Int,Int)) (BTree (Int,Int)) (BTree (Int,Int))

> addBTree :: Place -> BTree Place -> BTree Place  
> addBTree p Leaf = Branch p Leaf Leaf 
> addBTree p (Branch val tr1 tr2) | p < val     = Branch val (addBTree p tr1) tr2 
>                                 | otherwise   = Branch val tr1 (addBTree p tr2)

> inBTree :: Place -> BTree Place -> Bool 
> inBTree p Leaf                                = False
> inBTree p (Branch val tr1 tr2) | p == val     = True 
>                                | p < val      = inBTree p tr1 
>                                | otherwise    = inBTree p tr2 

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
>  in AMaze (x,y) (fa wallN Leaf) (fa wallS Leaf) (fa wallE Leaf) (fa wallW Leaf)
>       where   fa [] tr     = tr     
>               fa (x:xs) tr = fa xs (addBTree x tr) 

> reflect :: Wall -> Wall
> reflect ((i,j), d) = (move d (i,j), opposite d)

> hasWall :: Maze -> Place -> Direction -> Bool
> hasWall (AMaze _ wallN wallS wallE wallW) pos d   | d == N    = pos `inBTree` wallN
>                                                   | d == S    = pos `inBTree` wallS 
>                                                   | d == E    = pos `inBTree` wallE 
>                                                   | otherwise = pos `inBTree` wallW 

> sizeOf :: Maze -> Size
> sizeOf (AMaze size _ _ _ _) = size

