import GHC.Read (readField)
import Data.List.Split
import GHC.IO (unsafePerformIO)
import Data.List (sort)

-- file = "example"
file = "input"

day01 = do
  inputLines <- lines <$> readFile file
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  let a = map (splitOn "   ") inputLines
  let c1 = sort $ map (str2int . head ) a
  let c2 = sort $ map (str2int . \(a:b:c) -> b) a
  putStr "c1 = "
  print c1
  putStr "c2 = "
  print c2

  let d = zipWith diff c1 c2
  putStr "d = "
  print d

  print ""

  putStr "P1 = "
  print $ sum d

  let p2 = sum $ map (countIn c2) c1
  putStr "P2 = "
  print p2

countIn :: [Int] -> Int -> Int
countIn c2 e = e * length (filter (== e) c2)


diff :: Int -> Int -> Int
diff a b = abs $ a - b


str2int :: String -> Int
str2int = read

{-# NOINLINE main #-}
main = day01