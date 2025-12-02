module Day2 (main) where

import Misc (check,nub)
import Par4 (parse,Par,separated,lit,int)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day2.sample"
  inp <- parse gram <$> readFile "input/day2.input"
  print ("day2, part1 (sample)", check 1227775554 $ part1 sam)
  print ("day2, part1", check 22062284697 $ part1 inp)
  print ("day2, part2 (sample)", check 4174379265 $ part2 sam)
  print ("day2, part2", check 46666175279 $ part2 inp)
    where
      part1 = invalids [2]
      part2 = invalids [2..7]

invalids :: [Int] -> [Range] -> Int
invalids ws = sum . concat . map eachRange
  where
    eachRange (lo,hi) = nub $ concat [ cands w | w <- ws ]
      where
        cands w = takeWhile (<= hi) $ dropWhile (< lo) $
          [ x * rep
          | z <- [1::Int ..]
          , let rep = sum [ 10^(i*z) | i <- [0..w-1] ]
          , x <- [10^(z-1) .. 10^z - 1]
          ]

type Range = (Int,Int)

gram :: Par [Range]
gram = separated (lit ',') range
  where range = do a <- int; lit '-'; b <- int; pure (a,b)
