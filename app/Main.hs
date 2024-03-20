module Main (main) where

import Lib ( day1, day2, day3, day4, day5, day6, day7, day8, day9, day10 )


solutions = [ day1, day2, day3, day4, day5, day6, day7, day8, day9, day10 ]

runSolution :: (String -> String) -> String -> IO ()
runSolution solution fn = do
    sol <- solution <$> readFile ("data/" ++ fn)
    putStrLn $ show sol

main :: IO [()]
main = sequence . zipWith runSolution solutions $ map ((++".txt") . ("day"++) . show) [1..]


