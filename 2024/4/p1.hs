import Control.Applicative (liftA2)


type Matrix = [String]

offsets :: [[(Int, Int)]]
offsets = [
        [( d,  d) | d <- [0..3]],
        [( d, -d) | d <- [0..3]],
        [(-d,  d) | d <- [0..3]],
        [(-d, -d) | d <- [0..3]],
        [( d,  0) | d <- [0..3]],
        [(-d,  0) | d <- [0..3]],
        [( 0,  d) | d <- [0..3]],
        [( 0, -d) | d <- [0..3]]
    ]

addT (a1, a2) (b1, b2) = (a1+b1, a2+b2)


directions :: (Int,Int) -> [[(Int, Int)]]
directions pos =  map (map (addT pos)) offsets

checkDirection :: Matrix -> [(Int, Int)] -> Bool
checkDirection board poss = map (matrixGet board) poss == "XMAS"

countTrue = foldl (\c p -> if p then c+1 else c) 0

checkPos :: Matrix -> (Int, Int) -> Int
checkPos board pos = countTrue $ map (checkDirection board) (directions pos)
checkPosD board pos = map (checkDirection board) (directions pos)

matrixGet :: Matrix -> (Int, Int) -> Char
matrixGet board (x, y) = let len = length board in
                        if 0 <= x && x < len && 0 <= y && y < len
                            then (board!!x)!!y
                            else '.'

matrixCount :: Matrix -> Int
matrixCount board = let len =length board in
                    sum [checkPos board (x, y) | x <- [0 .. len], y <- [0 .. len]]

-- file = "example"
-- file = "example2"
file = "input"

day04 = do
    board <- lines <$> readFile file
    putStrLn $ unlines board

    print $ matrixCount board
    -- print $ checkPosD board (0,5)

    putStrLn ""