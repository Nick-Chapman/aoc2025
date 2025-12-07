module Day7 (main) where

import Misc (check)
import Par4 (parse,Par,separated,many,nl,lit,alts)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map.Strict qualified as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day7.sample"
  inp <- parse gram <$> readFile "input/day7.input"
  print ("day7, part1 (sample)", check 21 $ part1 sam)
  print ("day7, part1", check 1626 $ part1 inp)
  print ("day7, part2 (sample)", check 40 $ part2 sam)
  print ("day7, part2", check 48989920237096 $ part2 inp)

part1 :: Puz -> Int
part1 (start,rest) = loop 0 beams0 rest
  where
    beams0 = mkLine start

    loop :: Int -> Set Int -> [[Bool]] -> Int
    loop count beams = \case
      [] -> count
      bs:lines -> do
        let line = mkLine bs
        let hit = beams `Set.intersection` line
        let beams1 = beams `Set.difference` line
        let beams2 = Set.map (+1) hit
        let beams3 = Set.map (flip (-) 1) hit
        let beams' = beams1 `Set.union` beams2 `Set.union` beams3
        loop (count + Set.size hit) beams' lines

type MII = Map Int Int

part2 :: Puz -> Int
part2 (start,rest) = loop beams0 rest
  where
    beams0 = Map.fromList [ (p,1) | (p,x) <- zip [0..] start, x ]

    loop :: MII -> [[Bool]] -> Int
    loop beams = \case
      [] -> sum (Map.elems beams)
      bs:lines -> do
        let line = mkLine bs
        let hit = Map.restrictKeys beams line
        let beams1 = Map.withoutKeys beams line
        let beams2 = Map.mapKeys (+1) hit
        let beams3 = Map.mapKeys (flip (-) 1) hit
        let beams' = beams1 `merge` beams2 `merge` beams3
        loop beams' lines

    merge :: MII -> MII -> MII
    merge = Map.unionWith (+)

mkLine :: [Bool] -> Set Int
mkLine xs = Set.fromList [ p | (p,x) <- zip [0..] xs, x ]

type Puz = ([Bool],[[Bool]])

gram :: Par Puz
gram = splitHT <$> dropEven <$> separated nl (many cell)
  where
    cell = alts
      [ do lit 'S'; pure True
      , do lit '^'; pure True
      , do lit '.'; pure False
      ]

    splitHT = \case [] -> undefined; x:xs -> (x,xs)

    dropEven :: [a] -> [a]
    dropEven = \case [] -> []; x:_:xs -> x:dropEven xs; [_] -> undefined
