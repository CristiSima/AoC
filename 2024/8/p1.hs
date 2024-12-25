import Data.Set (Set)
import Data.Map (Map, empty)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor (bimap)

type Board = [String]
type Pos = (Int, Int)
type Size = (Int, Int)
type Check = Set Pos
-- type Frequency = (Char, Check)
type Frequencies = Map Char Check

enumerate :: [b] -> [(Int, b)]
enumerate = zip [0..]

positionalStream :: Board -> [(Pos, Char)]
positionalStream lines = concatMap linePosStream $ enumerate lines

linePosStream :: (Int, String) -> [(Pos, Char)]
linePosStream (x, b) = map (elPosStream x)  (enumerate b)

elPosStream :: Int -> (Int, Char) -> (Pos, Char)
elPosStream x (y, c)= ((x, y), c)

extractFrequencies :: Board -> Frequencies
extractFrequencies board = foldl (expandFrequencies) empty (positionalStream board)

expandFrequencies :: Frequencies -> (Pos, Char) -> Frequencies
expandFrequencies freq (pos, '.') = freq
expandFrequencies freq (pos, chr) = let oldPoss = M.findWithDefault S.empty chr freq in
                                    M.insert chr (S.insert pos oldPoss) freq

day08 file = do
    inputLines <- lines <$> readFile file
    let frequencies = extractFrequencies inputLines
    let size = (length inputLines, length $ head inputLines)
    let antinodes = foldl (antinodesFold size) S.empty frequencies
    putStr "P1: "
    print $ length antinodes

    let antinodes2 = foldl (antinodesFold2 size) S.empty frequencies
    putStr "P2: "
    -- print $ antinodes2
    print $ length antinodes2

    putStr ""

antinodesFold :: Size -> Check -> Check -> Check
antinodesFold size antinodes antenas = S.union antinodes $ S.unions $ map (constructAntinodes size antenas) (S.toList antenas)


constructAntinodes :: Size -> Check -> Pos -> Check
constructAntinodes size antenas anchor@(ax, ay) = let othreAntenas = S.delete anchor antenas in
                                            S.filter (isOnBoard size) $ S.map (bimap ((2 * ax) -) ((2 * ay) -)) othreAntenas




antinodesFold2 :: Size -> Check -> Check -> Check
antinodesFold2 size antinodes antenas = S.union antinodes $ S.unions $ map (constructAntinodes2 size antenas) (S.toList antenas)

constructAntinodes2 :: Size -> Check -> Pos -> Check
constructAntinodes2 size antenas anchor@(ax, ay) = let othreAntenas = S.delete anchor antenas in
                                            S.foldl (addHarmonics size anchor 0) S.empty  othreAntenas 

addHarmonics :: Size -> Pos -> Int -> Check -> Pos -> Check
addHarmonics size@(n,m) anchor@(ax, ay) order harmonics pos@(x,y) = let harmonic = (ax + order*(ax - x), ay + order*(ay - y)) in 
                                                            if isOnBoard size harmonic
                                                                then addHarmonics size anchor (order+1) (S.insert harmonic harmonics) pos
                                                                else harmonics
                                            -- S.filter (isOnBoard size) $ S.map (bimap ((2 * ax) -) ((2 * ay) -)) othreAntenas



isOnBoard :: Size -> Pos -> Bool
isOnBoard (n, m) (x, y) = 0 <= x && x < n && 0 <= y && y < m



test = day08 "example"
main = day08 "input"