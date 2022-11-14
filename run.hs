-- http://behalek.cs.vsb.cz/wiki/index.php/FP_Homework_2
module Main where
-- main = putStrLn "Hello, World!"

type Result = [String]
-- type Result = [Int]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

data Point = Point {x:: Int, y:: Int}
data Shape = Ellipse {s:: Point, a::Point, d:: Int} 
        |    Square {topLeft:: Point, size::Int}


loop :: Int -> Int
loop n = loop' n 0
    where loop' 0 a = a
          loop' n a = loop' (n - 1) (1 - a)

-- https://stackoverflow.com/questions/16004365/simple-haskell-loop

for list action = mapM_ action list

-- floop :: (Int, Int) -> IO() 
-- floop (x, y) = do    
--     for [0..x] (\ ix -> do
--         for [0..y] (\ iy -> do
--             putStr("x")
--             )
--         putStr("\n")
--         )

dot = "."
tag = "#"

isSquare :: (Int,Int) -> Shape -> IO()
isSquare (ix, iy) (Square (Point px py) s ) = do
    -- Once i made a filled square I was too lazy to think more and just copypasted the 
    -- if condition with 'and'
    if or[and[px + s >= ix, px == ix, py + s >= iy, py <= iy], 
        and[px + s >= ix, px <= ix, py + s >= iy, py == iy],
        and[px + s == ix, px <= ix, py + s >= iy, py <= iy],
        and[px + s >= ix, px <= ix, py + s == iy, py <= iy]]
        then putStr(tag)
        else putStr(dot)

isSquare (x,y) (Ellipse (Point sx sy)  (Point ax ay) d ) = do
    putStr(dot)

floop :: (Int, Int) -> IO() 
floop (x, y) = do    
    let sq = Square {topLeft = Point 15 5, size = 6 }
    for [0..y] (\ ix -> do
        for [0..x] (\ iy -> do
            isSquare (iy, ix) sq
            )
        putStr("\n")
        )

p = floop (40, 15)
-- floop x y = do    
--     for [0..x] (\ ix -> do
--         -- for [0..y] (\ iy -> do
--         print(iy)
--             -- )
--         )
    -- return 0 
-- floop = x   
--     for [0..x] (\ i -> do
--         print(i^2)
--         )
--         return 0 
          

view :: (Int,Int) -> [Point] -> Result
-- view :: (Int,Int) -> Result
view (x,y) p = [  ]
-- view (x,y) = ["asd"]
-- view (x,y) = ["asd"]

-- d = view (40,15) [Point 1 2]
-- d
    -- putStr (concat (map (++"\n") (view(4,15))))
    -- putStr (concat (map (++"\n") (view(4,15))))

-- xx = pp (view (1,1))
xx = view (1,1) [Point 1 2]
xxx = pp(xx)
main = p

-- pp( ["asddas"] ) 
-- view x y = IO()

-- view :: (Int,Int) -> [Shape] -> Result

-- pp(view (40,15) [Ellipse (Point 8 4) (Point 16 4) 6, Square {topLeft = Point 15 5, size = 6 }, Ellipse (Point 25 7) (Point 35 12) 7] )

-- main