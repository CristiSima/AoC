import Data.Map (Map)
import Data.Set (Set, empty, insert, fold, member, elems)
import Control.Concurrent (Chan)


data Direction = N | S | E | W deriving (Ord, Eq, Show);

type DPos = (Int, Int, Direction)
type Pos = (Int, Int)
type Size = (Int, Int)

type Obstacles = Set Pos
type Visited = Set Pos
type DVisited = Set DPos

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

positionalStream :: [String] -> [(Pos, Char)]
positionalStream lines = concatMap linePosStream $ enumerate lines

linePosStream :: (Int, String) -> [(Pos, Char)]
linePosStream (x, b) = map (elPosStream x)  (enumerate b)

elPosStream :: Int -> (Int, Char) -> (Pos, Char)
elPosStream x (y, c)= ((x, y), c)

getObstacles :: [(Pos, Char)] -> Obstacles
getObstacles = foldl posObstacle empty

posObstacle :: Obstacles -> (Pos, Char) -> Obstacles
posObstacle obs (p, c)= if c == '#' then insert p obs else obs

getStart :: [(Pos, Char)] -> Pos
getStart = foldl foldStart (-1, -1)

foldStart :: Pos -> (Pos, Char) -> Pos
foldStart p (np, c) = if c == '^' then np else p

visited :: Obstacles -> Pos -> Size -> Visited
visited obs pos = visitedN obs (insert pos empty) pos

goN (x, y) = (x-1, y  )
goS (x, y) = (x+1, y  )
goW (x, y) = (x  , y-1)
goE (x, y) = (x  , y+1)

isOutside :: Size -> Pos -> Bool
isOutside (n, m) (x, y) = x<0 || y<0 || x>=n || y>=m
isOutsideD :: Size -> DPos -> Bool
isOutsideD (n, m) (x, y, _) = x<0 || y<0 || x>=n || y>=m

--        obs       visited     pos   size   result 
visitedN :: Obstacles -> Visited -> Pos -> Size -> Visited
visitedN obs visited pos size
  | isOutside size pos = visited
  | member nPos obs = visitedE obs nVisited pos size
  | otherwise = visitedN obs nVisited nPos size
  where
        nVisited = insert pos visited
        nPos = goN pos

visitedE :: Obstacles -> Visited -> Pos -> Size -> Visited
visitedE obs visited pos size
  | isOutside size pos = visited
  | member nPos obs = visitedS obs nVisited pos size
  | otherwise = visitedE obs nVisited nPos size
  where
        nVisited = insert pos visited
        nPos = goE pos

visitedS :: Obstacles -> Visited -> Pos -> Size -> Visited
visitedS obs visited pos size
  | isOutside size pos = visited
  | member nPos obs = visitedW obs nVisited pos size
  | otherwise = visitedS obs nVisited nPos size
  where
        nVisited = insert pos visited
        nPos = goS pos

visitedW :: Obstacles -> Visited -> Pos -> Size -> Visited
visitedW obs visited pos size
  | isOutside size pos = visited
  | member nPos obs = visitedN obs nVisited pos size
  | otherwise = visitedW obs nVisited nPos size
  where
        nVisited = insert pos visited
        nPos = goW pos

isLoop :: Obstacles -> Pos -> Size -> Bool
isLoop obs (x, y) = isLoopD obs empty dPos
                    where
                        dPos = (x, y, N) 

forward :: DPos -> DPos
forward (x, y, N) = (x-1, y  , N)
forward (x, y, S) = (x+1, y  , S)
forward (x, y, W) = (x  , y-1, W)
forward (x, y, E) = (x  , y+1, E)

turnRight :: DPos -> DPos
turnRight (x, y, N) = (x, y, E)
turnRight (x, y, E) = (x, y, S)
turnRight (x, y, S) = (x, y, W)
turnRight (x, y, W) = (x, y, N)

--        obs       visited     pos   size   result 
isLoopD :: Obstacles -> DVisited -> DPos -> Size -> Bool
isLoopD obs visited pos size
  | isOutsideD size pos = False
  | member pos visited = True
  | member pfPos obs = isLoopD obs nVisited rPos size
  | otherwise = isLoopD obs nVisited fPos size
  where
        nVisited = insert pos visited
        fPos@(pfx, pfy, _) = forward pos
        pfPos = (pfx, pfy)
        rPos = turnRight pos


day06 file = do
    inputLines <- lines <$> readFile file
    let n = length inputLines
    let m = length $ head inputLines

    let poss = positionalStream inputLines
    let obs = getObstacles poss
    let start = getStart poss
    let visit = visited obs start (n,m)
    print start
    print (n,m)
    -- print $ visit
    putStr "P1: "
    print $ length visit

    putStr "P2: "
    print $ length $ filter id $ map (\pos -> isLoop (insert pos obs)  start (n,m)) $ elems visit
    putStr ""

test = day06 "example"
main = day06 "input"