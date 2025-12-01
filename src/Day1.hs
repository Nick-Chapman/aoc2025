module Day1 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,alts,lit,int)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day1.sample"
  inp <- parse gram <$> readFile "input/day1.input"
  print ("day1, part1 (sample)", check 3 $ part1 sam)
  print ("day1, part1", check 1147 $ part1 inp)
  print ("day1, part2 (sample)", check 6  $ part2 sam)
  print ("day1, part2", check 6789 $ part2 inp)

part1 :: [Rot] -> Int
part1 rots = length [ () | 0 <- loop 50 rots ]
  where
    loop :: Int -> [Rot] -> [Int]
    loop current rots = current : case rots of
      [] -> []
      L n : rots -> loop ((current-n) `mod` 100) rots
      R n : rots -> loop ((current+n) `mod` 100) rots

part2 :: [Rot] -> Int
part2 = part1 . concat . map expand
  where
    expand :: Rot -> [Rot]
    expand = \case
      L n -> replicate n (L 1)
      R n -> replicate n (R 1)

data Rot = L Int | R Int deriving Show

gram :: Par [Rot]
gram = separated nl line
  where line = alts [ do lit 'L'; L <$> int
                    , do lit 'R'; R <$> int ]
