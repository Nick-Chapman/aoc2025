module Day3 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,many,digit)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day3.sample"
  inp <- parse gram <$> readFile "input/day3.input"
  print ("day3, part1 (sample)", check 357 $ part1 sam)
  print ("day3, part1", check 17311 $ part1 inp)
  print ("day3, part2 (sample)", check 3121910778619 $ part2 sam)
  print ("day3, part2", check 171419245422055 $ part2 inp)
  where
    part1 = sum . map (joltage 2)
    part2 = sum . map (joltage 12)

joltage :: Int -> Bank -> Int
joltage numSWitchOn (Bank xs) = do
  loop (numSWitchOn-1) xs 0
  where
    loop :: Int -> [Int] -> Int -> Int
    loop i xs acc =
      if i < 0 then acc else do
        let dig = maximum (butLast i xs)
        let xs' = drop 1 $ dropWhile (/=dig) xs
        let acc' = 10*acc + dig
        loop (i-1) xs' acc'

butLast :: Int -> [a] -> [a]
butLast n = reverse . drop n . reverse

data Bank = Bank [Int] deriving Show

gram :: Par [Bank]
gram = separated nl line
  where line = Bank <$> many digit
