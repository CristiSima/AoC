{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
import Control.Monad
import Data.Tuple (swap)
import Text.ParserCombinators.ReadP (count)
import Data.Char (digitToInt)

type Q = Int
type Σ = Char

data NFA q σ = NFA
    { initial' :: q
    , transition' :: q -> σ -> [q]
    , isFinal' :: q -> Bool
    }

-- https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton#Example
-- p = 1, q = 2
endsWithOne :: NFA Q Σ
endsWithOne = NFA 1 transition (== 2)
  where
    transition q σ = case (q, σ) of
        (1, '0') -> [1]
        (1, '1') -> [1, 2]
        (2, '1') -> [2]
        _        -> []  -- missing transitions


runNFA :: NFA q σ -> [σ] -> [q]
runNFA (NFA init trans _) = foldl (\states char -> concatMap (`trans` char) states) [init]

acceptsNFA :: NFA q σ -> [σ] -> Bool
acceptsNFA nfa = any (isFinal' nfa) . runNFA nfa

countValidNFA :: NFA q σ -> [σ] -> Int
countValidNFA nfa text = length $ getValidNFA nfa text

getValidNFA :: NFA a σ -> [σ] -> [a]
getValidNFA nfa text = filter (isFinal' nfa)  (runNFA nfa text)

type QMulState = (Q, Int, Int)

buildMulState :: Int -> Int -> [Q] -> [QMulState]
buildMulState n1 n2 = map (\x -> (x,n1,n2))

--  1: start
--  123: 'mul'
--  4: '('
--  5: [0-9]    -> [6, 8]
--  6: [0-9]    -> [7, 8]
--  7: [0-9]
--  8: ','
--  9: [0-9]    -> [10,12]
-- 10: [0-9]    -> [11,12]
-- 11: [0-9]
-- 12: ')'      -> [13]
-- 13: accept
p1Mul :: NFA QMulState Σ
p1Mul = NFA (1, 0, 0) transition (\(s,_,_) -> s==13)
  where
    transition q σ = case (q, σ) of
        (( 1, n1, n2), 'm') -> buildMulState n1 n2 [1, 2]
        (( 1, n1, n2),  _ ) -> buildMulState n1 n2 [1]
        (( 2, n1, n2), 'u') -> buildMulState n1 n2 [3]
        (( 3, n1, n2), 'l') -> buildMulState n1 n2 [4]
        (( 4, n1, n2), '(') -> buildMulState n1 n2 [5]

        (( 5, n1, n2),  c ) -> maybe [] (\n -> buildMulState (n1*10+n) n2 [6, 8]) (parseDigit c)
        -- (( 5, n1, n2),  c ) -> if c `elem`  "123456789" then [6, 8] else []
        (( 6, n1, n2),  c ) -> maybe [] (\n -> buildMulState (n1*10+n) n2 [7, 8]) (parseDigit c)
        -- (( 6, n1, n2),  c ) -> if c `elem` "0123456789" then [7, 8] else []
        (( 7, n1, n2),  c ) -> maybe [] (\n -> buildMulState (n1*10+n) n2 [8]) (parseDigit c)
        -- (( 7, n1, n2),  c ) -> if c `elem` "0123456789" then [8] else []

        (( 8, n1, n2), ',') -> buildMulState n1 n2 [9]

        (( 9, n1, n2),  c ) -> maybe [] (\n -> buildMulState n1 (n2*10+n) [10, 12]) (parseDigit c)
        -- (( 9, n1, n2),  c ) -> if c `elem`  "123456789" then [10, 12] else []
        ((10, n1, n2),  c ) -> maybe [] (\n -> buildMulState n1 (n2*10+n) [11, 12]) (parseDigit c)
        -- ((10, n1, n2),  c ) -> if c `elem` "0123456789" then [11, 12] else []
        ((11, n1, n2),  c ) -> maybe [] (\n -> buildMulState n1 (n2*10+n) [12]) (parseDigit c)
        -- ((11, n1, n2),  c ) -> if c `elem` "0123456789" then [12] else []
        ((12, n1, n2), ')') -> buildMulState n1 n2 [13]
        ((13, n1, n2),  _ ) -> buildMulState n1 n2 [13]
        _        -> []  -- missing transitions

--  1: start
--  123: 'mul'
--  4: '('
--  5: [0-9]    -> [6, 8]
--  6: [0-9]    -> [7, 8]
--  7: [0-9]
--  8: ','
--  9: [0-9]    -> [10,12]
-- 10: [0-9]    -> [11,12]
-- 11: [0-9]
-- 12: ')'      -> [13]
-- 13: accept
-- 
--  1|22|23|..: "don't()"  -> 31
-- 31|32|33|34: "do()"     -> 1
p2Mul :: NFA QMulState Σ
p2Mul = NFA (1, 0, 0) transition (\(s,_,_) -> s==13)
  where
    transition q σ = case (q, σ) of
        -- parse mul
        (( 1, n1, n2), 'm') -> buildMulState n1 n2 [1, 2]
        (( 2, n1, n2), 'u') -> buildMulState n1 n2 [3]
        (( 3, n1, n2), 'l') -> buildMulState n1 n2 [4]
        (( 4, n1, n2), '(') -> buildMulState n1 n2 [5]

        (( 5, n1, n2),  c ) -> maybe [] (\n -> buildMulState (n1*10+n) n2 [6, 8]) (parseDigit c)
        -- (( 5, n1, n2),  c ) -> if c `elem`  "123456789" then [6, 8] else []
        (( 6, n1, n2),  c ) -> maybe [] (\n -> buildMulState (n1*10+n) n2 [7, 8]) (parseDigit c)
        -- (( 6, n1, n2),  c ) -> if c `elem` "0123456789" then [7, 8] else []
        (( 7, n1, n2),  c ) -> maybe [] (\n -> buildMulState (n1*10+n) n2 [8]) (parseDigit c)
        -- (( 7, n1, n2),  c ) -> if c `elem` "0123456789" then [8] else []

        (( 8, n1, n2), ',') -> buildMulState n1 n2 [9]

        (( 9, n1, n2),  c ) -> maybe [] (\n -> buildMulState n1 (n2*10+n) [10, 12]) (parseDigit c)
        -- (( 9, n1, n2),  c ) -> if c `elem`  "123456789" then [10, 12] else []
        ((10, n1, n2),  c ) -> maybe [] (\n -> buildMulState n1 (n2*10+n) [11, 12]) (parseDigit c)
        -- ((10, n1, n2),  c ) -> if c `elem` "0123456789" then [11, 12] else []
        ((11, n1, n2),  c ) -> maybe [] (\n -> buildMulState n1 (n2*10+n) [12]) (parseDigit c)
        -- ((11, n1, n2),  c ) -> if c `elem` "0123456789" then [12] else []
        ((12, n1, n2), ')') -> buildMulState n1 n2 [13]
        ((13, n1, n2),  _ ) -> buildMulState n1 n2 [13]

        (( 1, n1, n2),  c ) -> buildMulState n1 n2 $ if c == 'd'  then [22] else [1]
        ((22, n1, n2),  c ) -> buildMulState n1 n2 $ if c == 'o'  then [23] else [1]
        ((23, n1, n2),  c ) -> buildMulState n1 n2 $ if c == 'n'  then [24] else [1]
        ((24, n1, n2),  c ) -> buildMulState n1 n2 $ if c == '\'' then [25] else [1]
        ((25, n1, n2),  c ) -> buildMulState n1 n2 $ if c == 't'  then [26] else [1]
        ((26, n1, n2),  c ) -> buildMulState n1 n2 $ if c == '('  then [27] else [1]
        ((27, n1, n2),  c ) -> buildMulState n1 n2 $ if c == ')'  then [30] else [1]

        ((30, n1, n2),  c ) -> buildMulState n1 n2 $ if c == 'd'  then [31] else [30]
        ((31, n1, n2),  c ) -> buildMulState n1 n2 $ if c == 'o'  then [32] else [30]
        ((32, n1, n2),  c ) -> buildMulState n1 n2 $ if c == '('  then [33] else [30]
        ((33, n1, n2),  c ) -> buildMulState n1 n2 $ if c == ')'  then [ 1] else [30]


        _        -> []  -- missing transitions

parseDigit :: Char -> Maybe Int
parseDigit chr = if chr `elem` "0123456789"
                then Just $ digitToInt chr
                else Nothing

-- file = "example"
-- file = "example2"
file = "input"
day03 = do
  a <- readFile file
  let b = getValidNFA p1Mul a
  -- print b
  let c = map (\(_,n1,n2) -> n1*n2) b
  putStr "P1 = "
  print $ sum c
  print ""

  
  let d = getValidNFA p2Mul a
  -- print b
  let e = map (\(_,n1,n2) -> n1*n2) d
  putStr "P2 = "
  print $ sum e

main = day03