import GHC.Read (readField)
import Data.List.Split (splitOn)
import Data.List (inits)
import Data.Ix (Ix(inRange))

-- file = "example"
file = "input"

data CumulatedRaport = CumulatedRaport {
    last::Int,
    safeInc::Bool,
    safeDec::Bool
} deriving Show

startRaport :: Int -> CumulatedRaport
startRaport init = CumulatedRaport init True True

day02 = do
    inputLines <- lines <$> readFile file
    -- putStrLn $ unlines inputLines
    let a = map (splitOn " ") inputLines
    let b = map (map str2int) a

    let safes = map isChainSafe b
    -- print safes
    putStr "P1 = "
    print $ length (filter id safes)
    -- let b1:bs = map (map str2int) a
    -- let initStates = map startRaport b1
    -- print b1
    -- print initStates
    -- let finish = foldl updateStates initStates bs
    -- -- print finish
    -- print $ map (. stillSafe) finish

    let safesp2 = map isChainSafeP2 b
    -- print safesp2
    putStr "P2 = "
    print $ length (filter id safesp2)
    -- incorect answear for first interation
    print $ 358 < length (filter id safesp2)

    putStrLn ""


isChainSafeP2 :: [Int] -> Bool
isChainSafeP2 (nr:nrs) = let ends = foldl updateStateP2 [startRaport nr] nrs
                             with1 = any (\end -> safeInc end || safeDec end) ends
                        in with1 || isChainSafe nrs 
                        -- bc the first reported nr could be skiped

-- state:  no skip pressent
-- states: had a skip
updateStateP2 :: [CumulatedRaport] -> Int -> [CumulatedRaport]
updateStateP2 (state:states) current = updateState state current:(state:map ((\f -> f current) . updateState) states)

isChainSafe :: [Int] -> Bool
isChainSafe (nr:nrs) = let end = foldl updateState (startRaport nr) nrs
                        in safeInc end || safeDec end

updateStates :: [CumulatedRaport] -> [Int] -> [CumulatedRaport]
updateStates = zipWith updateState

updateState :: CumulatedRaport -> Int -> CumulatedRaport
updateState (CumulatedRaport last inc dec) current = CumulatedRaport current
                                                    (inc && inRange (1,3) (current - last) )
                                                    (dec && inRange (1,3) (last - current) )

diff :: Int -> Int -> Int
diff a b = abs $ a - b

str2int :: String -> Int
str2int = read

main = day02