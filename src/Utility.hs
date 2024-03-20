module Utility (snd3, concatTuples3, toInt, showIntPair,
                tuple3, adjacentPairs, Card(..), parseCard,
                Type(..), replace, dup, lcm, addPair, subPair) where


snd3 :: (a, b, c) -> b
snd3 (_, n, _)  = n

zipTupWith :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipTupWith f (a1, a2) (b1, b2) = (f a1 b1, f a2 b2)

addPair, subPair :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPair = zipTupWith (+)
subPair = zipTupWith (-)

concatTuples3 :: [(b, b, b)] -> [b]
concatTuples3   = (=<<) (\ (n, m, o) -> [n, m, o])

toInt :: String -> Int
toInt "zero"  = 0
toInt "one"   = 1
toInt "two"   = 2
toInt "three" = 3
toInt "four"  = 4
toInt "five"  = 5
toInt "six"   = 6
toInt "seven" = 7
toInt "eight" = 8
toInt "nine"  = 9
toInt a       = read a :: Int

showIntPair :: (Int, Int) -> String
showIntPair = show

tuple3 :: [c] -> (c, c, c)
tuple3 [a, b, c] = (a, b, c)
tuple3 _         = undefined

adjacentPairs :: [a] -> [(a, a)]
adjacentPairs (a : b : bs) = (a, b) : adjacentPairs bs
adjacentPairs _            = []

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x : xs)
    | a == x = b : replace a b xs
    | otherwise = x : replace a b xs

dup :: a -> (a, a)
dup a = (a, a)


data Card = I | II | III | IV | V | VI | VII | VIII | IX | T | J | Q | K | A  deriving (Eq, Ord, Show)
data Type = High | One | Two | Three | Full | Four | Five  deriving (Eq, Ord)


parseCard :: Char -> Maybe Card
parseCard c = case c of
    '1' -> Just I
    '2' -> Just II
    '3' -> Just III
    '4' -> Just IV
    '5' -> Just V
    '6' -> Just VI
    '7' -> Just VII
    '8' -> Just VIII
    '9' -> Just IX
    'T' -> Just T
    'J' -> Just J
    'Q' -> Just Q
    'K' -> Just K
    'A' -> Just A
    _   -> Nothing
