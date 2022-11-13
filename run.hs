-- http://behalek.cs.vsb.cz/wiki/index.php/FP_Homework_2
module Main where
main = putStrLn "Hello, World!"

type Result = [String]
-- type Result = [Int]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

data Point = Point Int Int
data Shape = Ellipse Point Point Int 
        | Square {topLeft:: Point, size::Int}


-- view :: (Int,Int) -> [Point] -> Result
view :: (Int,Int) -> Result
-- view (x,y) = ["asd"]
view (x,y) = ["asd"]

-- d = view (40,15) [Point 1 2]
-- d
    -- putStr (concat (map (++"\n") (view(4,15))))
    -- putStr (concat (map (++"\n") (view(4,15))))

-- xx = pp (view (1,1))
xx = view (1,1)
xxx = pp(xx)

-- pp( ["asddas"] ) 
-- view x y = IO()

-- view :: (Int,Int) -> [Shape] -> Result

-- pp(view (40,15) [Ellipse (Point 8 4) (Point 16 4) 6, Square {topLeft = Point 15 5, size = 6 }, Ellipse (Point 25 7) (Point 35 12) 7] )

-- main