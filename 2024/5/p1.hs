import Data.List.Split (splitOn)
-- import Data.IntMap (IntMap, empty, lookup, insert)
import qualified Data.IntMap as N
import Data.Maybe (fromMaybe)
import Data.Set (Set, empty, insert, member)
import Numeric (readInt)
import Data.List ((\\))

type DB = Set (Int, Int)

buildRules :: [String] -> DB
buildRules = foldl bbb2 empty

bbb2 :: DB -> String -> DB
bbb2 db aaa = let [a1,a2] = splitOn "|" aaa
                  a = parseInt a1
                  b = parseInt a2 in
        insert (a,b) db

parseInt :: String -> Int
parseInt = read

isCorrect :: DB -> [Int] -> Bool
isCorrect db [n] = True
isCorrect db (n:ns) = all (\x -> member (n,x) db) ns && isCorrect db ns

isIncorrect :: DB -> [Int] -> Bool
isIncorrect db ns = not $  isCorrect db ns

mid :: [a] -> a
mid xs = xs !! (l `div` 2 )
  where l = length xs


fixOrder :: DB -> [Int] -> [Int]
fixOrder db [n] = [n]
fixOrder db (n:ns) = if all (\x -> member (n,x) db) ns
                     then n:fixOrder db ns
                     else let newN = foldl (\n x -> if member (n,x) db then n else x) n ns in
                          newN:fixOrder db ((n:ns) \\ [newN])


day05 file = do
    inputLines <- lines <$> readFile file
    let poRules:updatesS:_ = splitOn [""] inputLines
    -- print poRules
    let rules = buildRules poRules
    -- print rules

    let updates = map (map parseInt . splitOn "," ) updatesS

    -- print updates
    -- print $ map (isCorrect rules) updates

    putStr "P1 = "
    print $ sum $ map mid $ filter (isCorrect rules) updates

    putStr "P2 = "
    print $ sum $ map (mid . fixOrder rules) (filter (isIncorrect rules) updates)


test = day05 "example"
main = day05 "input"