> import Geography
> import MyMaze

======================================================================

Draw a maze.

***************************************
*              Question 2             *
* Complete the definition of drawMaze *
***************************************

> drawMaze :: Maze -> IO()
> drawMaze maze = putStr(concat (map f (reverse [0..(c-1)])) ++ nsRow maze (-1) (r-1) ++ "\n" )
>       where   f x = nsRow maze x (r-1) ++ "\n" ++ ewRow maze x (r-1) ++ "\n"
>               (r,c) = sizeOf maze

> ewRow :: Maze -> Int -> Int -> String 
> ewRow m r (-1)    | hasWall m (0,r) W = "|"
>                   | otherwise         = " " 
> ewRow m r n       | hasWall m (n,r) E = ewRow m r (n-1) ++ "  |"
>                   | otherwise         = ewRow m r (n-1) ++ "   "

> nsRow :: Maze -> Int -> Int -> String
> nsRow m r (-1)    = "+"
> nsRow m r n       | hasWall m (n,r) N = nsRow m r (n-1) ++ "--+"
>                   | otherwise         = nsRow m r (n-1) ++ "  +"  

======================================================================

Solve the maze, giving a result of type:

> type Path = [Direction]

***************************************
*            Questions 3--4           *
*     Complete the definition of      *
*              solveMaze              *
***************************************

> solveMaze :: Maze -> Place -> Place -> Path
> solveMaze m (sx,sy) (tx,ty) = fastSolveMazeIter m (tx,ty) [] [((sx,sy),[])] 

> solveMazeIter :: Maze -> Place -> [(Place,Path)] -> Path 
> solveMazeIter m p xs          | length fp > 0     = snd (head fp)
>                               | otherwise         = solveMazeIter m p (concat [(fm m (v,w)) | (v,w) <- xs])
>               where fp = filter (\(x,y) -> x == p) xs

> fastSolveMazeIter :: Maze -> Place -> [Place] -> [(Place,Path)] -> Path 
> fastSolveMazeIter m p v xs    | xs == []          = error "No Paths exist"
>                               | length fp > 0     = snd (head fp)
>                               | otherwise         = fastSolveMazeIter m p (map fst xs ++ v) (remdup (concat [filter ff (fm m (v,w)) | (v,w) <- xs]))
>               where fp = filter (\(x,y) -> x == p) xs
>                     ff (vl,vr) = not (elem vl v) 
>                     remdup [] = []
>                     remdup (x:xs) = x:remdup (remove x xs)
>                           where remove x [] = []
>                                 remove (xl,xr) ((yl,yr):ys)   | xl == yl = remove (xl,xr) ys
>                                                               | otherwise = (yl,yr) : remove (xl,xr) ys

> fm :: Maze -> (Place,Path) -> [(Place,Path)]
> fm m ((vx,vy),w) =  filter (\(x,y) -> y /= []) [
>       if hasWall m (vx,vy) E then ((0,0),[]) else ((vx+1,vy),w++[E]),
>       if hasWall m (vx,vy) W then ((0,0),[]) else ((vx-1,vy),w++[W]),
>       if hasWall m (vx,vy) N then ((0,0),[]) else ((vx,vy+1),w++[N]),
>       if hasWall m (vx,vy) S then ((0,0),[]) else ((vx,vy-1),w++[S])
>       ]

======================================================================

Some test mazes.  In both cases, the task is to find a path from the bottom
left corner to the top right.

First a small one

> smallMaze :: Maze
> smallMaze = 
>   let walls = [((0,0), N), ((2,2), E), ((2,1),E), ((1,0),E), 
>                ((1,2), E), ((1,1), N)]
>   in makeMaze (4,3) walls

Now a large one.  Define a function to produce a run of walls:

> run (x,y) n E = [((x,y+i),E) | i <- [0..n-1]]
> run (x,y) n N = [((x+i,y),N) | i <- [0..n-1]]

And here is the maze.

> largeMaze :: Maze 
> largeMaze =
>   let walls = 
>         run (0,0) 3 E ++ run (1,1) 3 E ++ [((1,3),N)] ++ run (0,4) 5 E ++
>         run (2,0) 5 E ++ [((2,4),N)] ++ run (1,5) 3 E ++
>         run (1,8) 3 N ++ run (2,6) 3 E ++
>         run (3,1) 7 E ++ run (4,0) 4 N ++ run (4,1) 5 E ++ run (5,2) 3 N ++
>         run (4,6) 2 N ++ run (5,4) 3 E ++ run (6,3) 5 N ++ run (8,0) 4 E ++
>         run (6,1) 3 N ++ run (0,9) 3 N ++ run (1,10) 3 N ++ run (0,11) 3 N ++
>         run (1,12) 6 N ++ run (3,9) 4 E ++ run (4,11) 2 N ++
>         run (5,9) 3 E ++ run (4,8) 3 E ++ run (5,7) 5 N ++ run (6,4) 9 E ++
>         run (7,5) 3 N ++ run (8,4) 4 N ++ run (8,6) 3 N ++ run (10,5) 7 E ++
>         run (9,8) 3 E ++ run (8,9) 3 E ++ run (7,8) 3 E ++ run (8,11) 3 N ++
>         run (0,13) 5 N ++ run (4,14) 2 E ++ run (0,15) 2 E ++ 
>         run (1,14) 3 N ++ run (3,15) 2 E ++ run (0,17) 2 N ++ 
>         run (1,16) 2 E ++ run (2,15) 1 N ++ run (3,16) 3 N ++
>         run (2,17) 2 E ++ run (1,18) 6 N ++ run (4,17) 3 N ++ 
>         run (6,14) 7 E ++ run (5,13) 4 E ++ run (7,12) 2 E ++
>         run (8,13) 3 N ++ run (7,14) 3 N ++ run (10,14) 2 E ++
>         run (8,15) 5 N ++ run (7,16) 5 N ++ run (9,1) 2 E ++
>         run (10,0) 12 N ++ run (21,1) 1 E ++ run (10,2) 2 E ++
>         run (11,1) 7 N ++ run (17,1) 1 E ++ run (11,3) 3 E ++
>         run (12,2) 7 N ++ run (18,2) 2 E ++ run (19,1) 2 N ++
>         run (15,3) 3 N ++ run (14,4) 3 E ++ run (13,3) 3 E ++
>         run (12,4) 3 E ++ run (12,6) 3 N ++ run (11,7) 8 E ++ 
>         run (9,12) 3 N ++ run (12,14) 1 N ++ run (12,8) 10 E ++
>         run (0,19) 6 N ++ run (1,20) 6 N ++ run (7,18) 8 E ++
>         run (8,17) 1 N ++ run (8,18) 3 E ++ run (9,17) 4 E ++ 
>         run (10,18) 2 E ++ run (11,17) 2 E ++ run (10,20) 3 N ++
>         run (11,19) 3 N ++ run (12,18) 2 N ++ run (13,17) 2 N ++
>         run (13,13) 4 E ++ run (14,12) 7 N ++ run (13,11) 2 N ++
>         run (14,10) 2 E ++ run (13,9)2 E ++ run (14,8) 3 N ++ 
>         run (13,7) 3 N ++ run (15,5) 3 E ++ run (16,6) 3 E ++
>         run (18,5) 4 N ++ run (16,4) 2 N ++ run (13,20) 2 E ++
>         run (14,18) 4 E ++ run (20,2) 3 N ++ run (19,3) 2 E ++
>         run (18,4) 2 E ++ run (23,4) 1 E ++ run (22,4) 1 N ++
>         run (21,3) 1 N ++ run (20,4) 2 E ++ run (17,6) 4 N ++ 
>         run (20,7) 2 E ++ run (21,7) 2 N ++ run (21,6) 1 E ++ 
>         run (15,9) 1 E ++ run (17,8) 2 E ++ run (18,7) 2 E ++ 
>         run (19,8) 2 E ++ run (21,9) 1 E ++ run (16,9) 6 N ++
>         run (16,10) 7 N ++ run (15,11) 2 E ++ run (17,11) 5 N ++ 
>         run (14,14) 3 E ++ run (15,15) 6 E ++ run (17,14) 4 E ++
>         run (16,18) 4 E ++ run (15,17) 1 N ++ run (17,17) 3 N ++
>         run (15,13) 7 N ++ run (21,12) 2 E ++ run (16,16) 1 N ++
>         run (16,14) 1 N ++ run (17,15) 3 N ++ run (19,14) 4 N ++
>         run (20,15) 5 E ++ run (19,16) 2 N ++ run (21,16) 5 E ++
>         run (17,19) 2 E ++ run (18,20) 2 E ++ run (19,19) 2 E ++
>         run (18,18) 2 N ++ run (20,20) 3 N
>   in makeMaze (23,22) walls

And now an impossible maze

> impossibleMaze :: Maze
> impossibleMaze =
>   let walls = [((0,1), E), ((1,0),N), ((1,2), E), ((2,1), N)]
>   in makeMaze (3,3) walls


Exercise 5
Added Empty Maze

> emptyMaze :: Maze 
> emptyMaze =
>   let walls = []
>   in makeMaze (100,100) walls

Other Mazes

> squigglyMaze :: Maze 
> squigglyMaze = 
>   let walls = run (0,0) 10 E ++ run (1,1) 10 E ++ run (2,0) 10 E ++ run (3,1) 10 E ++ run (4,0) 10 E
>            ++ run (5,1) 10 E ++ run (6,0) 10 E ++ run (7,1) 10 E ++ run (8,0) 10 E ++ run (9,1) 10 E
>   in makeMaze (11,11) walls 

> squigglyMaze2 :: Maze 
> squigglyMaze2 = 
>   let walls = run (0,0) 10 E ++ run (1,1) 10 E ++ run (2,0) 10 E ++ run (3,1) 10 E ++ run (4,0) 10 E
>            ++ run (5,1) 10 E ++ run (6,0) 10 E ++ run (7,1) 10 E ++ run (8,0) 10 E ++ run (9,1) 10 E
>            ++ [((10,9), N)]
>   in makeMaze (11,11) walls 