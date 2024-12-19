import Text.Read.Lex (expect)
import Data.List.Split (splitOn)


type LineInfo = (Int, [Int])

check :: Int -> Int -> [Int] -> Bool
check expected accumulator [] = expected == accumulator
check expected accumulator (x:xs) = check expected (accumulator + x) xs
                                    || check expected (accumulator * x) xs

isValid :: LineInfo -> Bool
isValid (expected, xs)= check expected 0 xs


concatI :: Int -> Int -> Int
concatI a b = mult a b + b
            where
                mult acc 0 = acc
                mult acc nr = mult (acc*10) (nr `div` 10)

check2 :: Int -> Int -> [Int] -> Bool
check2 expected accumulator [] = expected == accumulator
check2 expected accumulator (x:xs) = check2 expected (accumulator + x) xs
                                    || check2 expected (accumulator * x) xs
                                    || check2 expected (concatI accumulator x) xs

isValid2 :: LineInfo -> Bool
isValid2 (expected, x:xs)= check2 expected x xs

parseLine :: String -> LineInfo
parseLine line = (read p1, map read ps)
                where
                    [p1, p2] = splitOn ": " line
                    ps = splitOn " " p2

day07 file = do
    inputLines <- lines <$> readFile file
    let lines = map parseLine inputLines

    let p1 = sum $ map fst $ filter isValid lines
    putStr "P1: "
    print p1

    let p2 = sum $ map fst $ filter isValid2 lines
    putStr "P2: "
    print p2
    
    putStr ""


test = day07 "example"
main = day07 "input"