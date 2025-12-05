module Day5 (main) where

import Misc (check)
import Par4 (parse,Par,separated,terminated,nl,lit,int)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day5.sample"
  inp <- parse gram <$> readFile "input/day5.input"
  print ("day5, part1 (sample-detail)", check [5,11,17] $ part1 sam)
  print ("day5, part1 (sample)", check 3 $ length $ part1 sam)
  print ("day5, part1", check 821 $ length $ part1 inp)
  print ("day5, part2 (sample)", check 14 $ (sum . map rangeSize) $ part2 sam)
  print ("day5, part2", check 344771884978261 $ (sum . map rangeSize) $ part2 inp)

part1 :: Puz -> [Int]
part1 (Puz ranges xs) = [ x | x <- xs, inSomeRange x ]
  where inSomeRange x = any id [ inRange r x | r <- ranges ]

part2 :: Puz -> [Range]
part2 (Puz ranges _) = foldl mergeRange [] ranges

mergeRange :: [Range] -> Range -> [Range]
mergeRange rs r1@(lo1,hi1) =
  case rs of
    [] -> [r1]
    r2@(lo2,hi2):rest -> do
      if hi1 < lo2 then r1 : r2 : rest else
        if hi2 < lo1 then r2 : mergeRange rest r1 else
          mergeRange rest (min lo1 lo2, max hi1 hi2)

inRange :: Range -> Int -> Bool
inRange (lo,hi) x = lo <= x && x <= hi

rangeSize :: Range -> Int
rangeSize (lo,hi) = hi-lo+1

data Puz = Puz [Range] [Int] deriving Show
type Range = (Int,Int)

gram :: Par Puz
gram = do
  rs <- terminated nl range; nl
  xs <- separated nl int
  pure (Puz rs xs)
  where range = do a <- int; lit '-'; b <- int; pure (a,b)

