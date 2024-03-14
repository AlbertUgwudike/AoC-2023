module Lib ( day1, day2, day3, day4, day5, day6, day7, day8 ) where

import           Data.Bifunctor       (Bifunctor (first), bimap, second)
import           Data.Char            (isAlpha, isDigit, ord)
import           Data.Function        (on)
import           Data.Functor.Classes (Ord1 (liftCompare))
import           Data.List            (group, groupBy, intercalate, maximumBy,
                                       singleton, sort, sortBy)
import           Data.Maybe           (fromMaybe, listToMaybe)
import           Text.Regex.PCRE      (getAllTextMatches, (=~))
import           Utility              (Card (..), Type (..), adjacentPairs,
                                       concatTuples3, dup, parseCard, replace,
                                       showIntPair, snd3, toInt, tuple3)


import qualified Data.IntMap          as IntMap
import qualified Data.IntSet          as IntSet


day8 :: String -> String
day8 s = show (sol1, sol2) -- (21409, 21165830176709)
    where
        toEntry (k, l, r) = (k, (l, r))
        parseEdge         = filter (all isAlpha) . groupBy (on (==) isAlpha)
        convert           = toEntry . tuple3 . map hash . parseEdge
        (dirs, edges)     = bimap (cycle . head) (map convert . drop 2) . dup $ lines s
        graph             = IntMap.fromList edges
        allA              = filter ((== 0) . (`mod` 26)) . map fst $ edges

        sol1 = find graph dirs (hash "AAA")
        sol2 = foldl1 lcm $ map (rhythm graph IntMap.empty (zip dirs [0..])) allA

hash :: [Char] -> Int
hash = sum . zipWith (*) [26 * 26, 26, 1] . map (flip (-) (ord 'A') . ord)

find :: IntMap.IntMap (Int, Int) -> String -> Int -> Int
find _ [] _ = 0
find m (d:ds) k
    | k == hash "ZZZ" = 0
    | d == 'L'        = 1 + find m ds l
    | otherwise       = 1 + find m ds r
    where (l, r) = m IntMap.! k

rhythm :: IntMap.IntMap (Int, Int) -> IntMap.IntMap [Int] -> [(Char, Int)] -> Int -> Int
rhythm _ _ [] _  = undefined
rhythm graph visitedGraph ((dir, currPos) : dirs) start
    | alreadyVisited = currPos - pos -- this is a massive coincidence
    | otherwise      = rhythm graph updatedVisited dirs updatedStart
    where
        visited         = fromMaybe [] (visitedGraph IntMap.!? start)
        alreadyVisited  = (currPos `mod` 271) `elem` map (`mod` 271) visited
        pos             = head $ filter (\a -> a `mod` 271 == currPos `mod` 271) visited
        updatedVisited  = IntMap.insert start (currPos : visited) visitedGraph
        selector        = if dir == 'L' then fst else snd
        updatedStart    = selector $ graph IntMap.! start


day7 :: String -> String
day7 s = show (sol id compare bets, sol promote comp' bets)  -- 246912307, 246894760
    where
        longestGroup   = maximumBy (on compare length) . ([]:) . group . sort . filter (/=J)
        abundant       = fromMaybe J . listToMaybe . longestGroup
        promote        = uncurry (replace J) . first abundant . dup
        sortBets t cmp = sortBy (on (compareHand t cmp) fst)
        sol t cmp bs   = sum . flip (zipWith (*)) [1..] . map snd $ sortBets t cmp bs
        bets           = map (bimap (fromMaybe [] . mapM parseCard) toInt) . adjacentPairs $ words s

compareHand :: ([Card] -> [Card]) -> (Card -> Card -> Ordering) -> [Card] -> [Card] -> Ordering
compareHand t cardComp h1 h2
    | htt h1 == htt h2 = liftCompare cardComp h1 h2
    | otherwise = compare (htt h1) (htt h2)
    where htt = handToType t

handToType :: ([Card] -> [Card]) -> [Card] -> Type
handToType f hand = case product . map ((+1) . length) . group . sort . f $ hand of
    6  -> Five
    10 -> Four
    12 -> Full
    16 -> Three
    18 -> Two
    24 -> One
    _  -> High

comp' :: Card -> Card -> Ordering
comp' J J   = EQ
comp' J _   = LT
comp' _ J   = GT
comp' c1 c2 = compare c1 c2


day6 :: String -> String
day6 s = showIntPair (sol1, sol2) -- (1083852,23501589)
    where
        rows  = lines s
        times = tail . words . head $ rows
        dists = tail . words . last $ rows
        sol1  = product $ zipWith day6_1 (map toInt times) (map toInt dists)
        sol2  = day6_1 (toInt $ concat times) (toInt $ concat dists)

day6_1 :: Int -> Int -> Int
day6_1 t d = if disc < 0 then 0 else x2 - x1 + 1
    where
        disc = (t * t) - 4 * d
        u    = sqrt (fromIntegral disc) / 2 :: Double
        x1   = ceiling $ fromIntegral t / 2 - u
        x2   = floor $ fromIntegral t / 2 + u


day5 :: String -> String
day5 s = show (sol1 seeds, sol2 seeds) -- (84470622, 26714516)
    where
        rows    = lines s
        seeds   = map toInt . tail . words $ head rows
        chunks  = filter (/=[""]) . groupBy (on (&&) (/="")) $ tail rows
        maps    = map (sortBy (on compare snd3) . map (tuple3 . map toInt . words) . tail) chunks
        sol1    = minimum . map (flip (foldl day5_1) maps)
        f rs m  = rs >>= day5_2 m
        sol2    = minimum . concatMap (map fst . flip (foldl f) maps . singleton) . adjacentPairs

day5_1 :: Int -> [(Int, Int, Int)] -> Int
day5_1 s [] = s
day5_1 s ((a, b, c) : as)
    | s >= b && s < b + c   = s - b + a
    | otherwise             = day5_1 s as

day5_2 :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
day5_2 [] s = [s]
day5_2 (transform : ms) s = uncurry (++) . second (concatMap (day5_2 ms)) $ splitRange s transform

splitRange :: (Int, Int) -> (Int, Int, Int) -> ([(Int, Int)], [(Int, Int)])
splitRange (a, b) (c, d, e)
    | lowLow  && highLow  = ([(a, b)]                     , []                      )
    | lowLow  && highIn   = ([(a, d - a), (c, a + b - d)] , []                      )
    | lowLow  && highHigh = ([(a, d - a), (c, e)]         , [(d + e, a + b - d - e)])
    | lowIn   && highIn   = ([(a - d + c, b)]             , []                      )
    | lowIn   && highHigh = ([(a - d + c, d + e - a)]     , [(d + e, a + b - d - e)])
    | lowHigh && highHigh = ([]                           , [(a, b)]                )
    | otherwise           = ([] , [])
    where
        lowIn    = a >= d && a < d + e
        lowLow   = a < d
        lowHigh  = a >= d + e
        highHigh = a + b >= d + e
        highIn   = a + b <  d + e
        highLow  = a + b < d


day4 :: String -> String
day4 s = showIntPair $ day4' stack cards -- (20117, 13768818)
    where
        split str   = (takeWhile (/='|') str, drop 1 $ dropWhile (/='|') str)
        nums        = map toInt . words
        parse       = bimap nums nums . split . drop 1 . dropWhile (':'/=)
        cards       = map parse $ lines s
        stack       = replicate (length cards) 0

day4' :: [Int] -> [([Int], [Int])] -> (Int, Int)
day4' _ [] = (0, 0)
day4' stack ((winning, ticket) : rest)
    | matches == 0  = (s1, copies + s2)
    | otherwise     = (2 ^ (matches - 1) + s1, copies + s2)
    where
        winningSet  = IntSet.fromList winning
        matches     = length . filter id $ map (`IntSet.member` winningSet) ticket
        copies      = 1 + head stack
        sstack      = drop 1 stack
        copyList    = replicate matches copies ++ replicate (length sstack - matches) 0
        newStack    = zipWith (+) copyList sstack
        (s1, s2)    = day4' newStack rest



day3 :: String -> String
day3 s = showIntPair (sol1, sol2) -- (533784, 78826761)
    where
        sol1 = day3_1 (process s)
        sol2 = day3_2 ([], [], []) (process s)

day3_1 :: (String, String, String) -> Int
day3_1 (a' : a : as,  b': b : bs,  c' : c : cs)
    | not . isDigit $ b = day3_1 (a : as, b : bs, c : cs)
    | not $ any isEnginePart surrounding = day3_1 (unzip3 rest)
    | otherwise = number + day3_1 (unzip3 rest)
    where
        isEnginePart n  = not (isDigit n || n == '.')
        digitTuples     = (a, b, c) : takeWhile (isDigit . snd3) (zip3 as bs cs)
        rest            = dropWhile (isDigit . snd3) (zip3 as bs cs)
        number          = read $ map snd3 digitTuples
        surrounding     = concatTuples3 $ [(a', b', c')] ++ digitTuples ++ take 1 rest

day3_1 _ = 0

day3_2 :: (String, String, String) -> (String, String, String) -> Int
day3_2 (s, t, u) (a:as, b:bs, c:cs)
    | b /= '*' || length numbers <= 1 = rest
    | otherwise = product numbers + rest
    where
        parse str   = [takeWhile isDigit str, drop 1 $ dropWhile isDigit str]
        after       = map (takeWhile isDigit) [as, bs, cs]
        prior       = map (reverse . takeWhile isDigit) [s, t, u]
        rows        = zipWith3 (\ n m o -> n ++ [m] ++ o) prior [a, b, c] after
        numbers     = map read . filter (not . null) . (>>= parse) $ rows
        rest        = day3_2 (a:s, b:t, c:u) (as, bs, cs)

day3_2 _ _ = 0

process :: String -> (String, String, String)
process s = (dropRows 0, dropRows 1, dropRows 2)
    where
        rows       = lines s
        pad        = replicate (length rows + 1) '.'
        padRows    = [pad] ++ map (++ ".") rows ++ [pad]
        dropRows n = concat $ drop n padRows

match :: String -> String -> [String]
match re s = getAllTextMatches $ s =~ re :: [String]

firstMatch :: String -> String -> String
firstMatch = flip (=~)

day2 :: String -> String
day2 s = showIntPair (sol1 s, sol2 s) -- (2061, 72596)
    where
        re = "\\d+\\s(red|green|blue)"

        valid t = case words t of
            [a, "red"]   -> toInt a <= 12
            [a, "green"] -> toInt a <= 13
            [a, "blue"]  -> toInt a <= 14
            _            -> False

        optimal (r, g, b) t = case words t of
            [a, "red"]   -> (max r $ read a, g, b)
            [a, "green"] -> (r, max g $ read a, b)
            [a, "blue"]  -> (r, g, max b $ read a)
            _            -> (r, g, b)

        sol1 = sum . map fst . filter snd . zip [1..] . map (all valid . match re) . lines
        sol2 = sum . map ((\(r, g, b) -> r * g * b) . foldl optimal (0, 0, 0) . match re) . lines


day1 :: String -> String
day1 s = show (sol (f "\\d") (l "\\d"), sol (f re) (l re')) -- (55017, 53539)
    where
        ints  = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

        re  = "(" ++ intercalate "|" ints ++ "|\\d)"
        re' = "(" ++ intercalate "|" (map reverse ints) ++ "|\\d)"

        f r = (10*) . toInt . firstMatch r
        l r = toInt . reverse . firstMatch r . reverse

        sol fFunc lFunc = sum . uncurry (zipWith (+)) . bimap (map fFunc) (map lFunc) . dup $ lines s
