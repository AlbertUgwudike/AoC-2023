module Main (main) where

import Lib ( day1, day2, day3, day4, day5, day6, day7, day8 )


runSolution :: Show a => (String -> a) -> String -> IO ()
runSolution solution fn = do
    sol <- solution <$> readFile ("data/" ++ fn)
    putStrLn $ show sol

main :: IO ()
main = do
    runSolution day1 "day1.txt"
    runSolution day2 "day2.txt"
    runSolution day3 "day3.txt"
    runSolution day4 "day4.txt"
    runSolution day5 "day5.txt"
    runSolution day6 "day6.txt"
    runSolution day7 "day7.txt"
    runSolution day8 "day8.txt"


