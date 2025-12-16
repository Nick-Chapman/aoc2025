module Day10 (main) where

import Misc (check,hist,collate)
import Par4 (parse,Par,many,terminated,separated,alts,int,lit,nl)
import Data.Map qualified as Map
import Data.Map (Map)
import Text.Printf (printf)
import Data.Ord (comparing)

import Control.Monad (forM_)
import Data.List (sortBy)
import GHC.Int (Int64)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day10.sample"
  inp <- parse gram <$> readFile "input/day10.input"
  part1_sam <- part1 sam
  print ("day10, part1 (sample-detail)", check [2,3,2] $ part1_sam)
  print ("day10, part1 (sample)", check 7 $ sum $ part1_sam)
  part1_inp <- part1 inp
  print ("day10, part1", check 461 $ sum $ part1_inp)

  sam_details <- part2 (zip [1000..] sam)
  let _sam_xs = [ answer | Res{answer} <- sam_details ]
  print ("day10, part2 (sample-detail)", check [10,12,11] $ _sam_xs)

  let expected :: [Int] = [128,42,80,9,75,101,80,32,101,219,91,186,59,54,243,90,138,245,64,127,50,59,88,34,160,163,108,101,69,127,69,57,233,52,47,51,49,64,44,52,69,215,307,249,106,185,84,95,88,38,186,11,65,132,104,64,140,200,62,25,66,93,41,82,55,74,62,108,79,105,77,36,52,65,93,50,89,41,195,177,96,116,63,211,68,85,223,56,58,47,40,7,80,30,74,103,52,64,19,138,98,26,196,53,65,112,60,114,94,48,54,88,233,159,79,58,66,283,135,103,114,111,37,127,257,49,70,103,36,26,88,145,39,107,80,76,161,100,171,48,53,215,64,244,80,96,62,102,24,123,105,57,65,70,72,64,89,83,104,50,177,59,229,62,95,58,279]

  let all = [0::Int ..166]
  let _hard = [164,67,118,131,124,99,166] -- slowest 7 take 13 seconds
  let _easy = [ i | i <- all, i `notElem` _hard ] -- rest take just 2 seconds

  let pick = all

  let iPicked = [ (i,x) | (i,x) <- zip [0::Int ..] inp, i `elem` pick ]
  let xPicked = [ x | (i,x) <- zip [0::Int ..] expected, i `elem` pick ]

  details <- part2 iPicked
  let xs = [ answer | Res{answer} <- details ]

  _printTimings [ (duration,info) | Res{duration,info} <- details ]

  print ("day10, part2 (detail)", check xPicked $ xs)
  print ("day10, part2", check (sum xPicked) $ sum xs)
  --print ("day10, part2", check 16386 $ sum xs)
  pure ()


part1 :: [Machine] -> IO [Int]
part1 = mapM solve1

solve1 :: Machine -> IO Int
solve1 Machine{lights,buttons} = do
  let goal = [ n | (n,b) <- zip [0::Int ..] lights, b]
  let cands = power (length buttons)
  let
    testCand bs = do
      let bbs = zip buttons bs
      let collapse = [ x | (Button xs,bool) <- bbs, bool, x<- xs ]
      let xns = Map.toList (hist collapse)
      let odds = [ x | (x,n) <- xns, n `mod` 2 == 1 ]
      let match = (odds == goal)
      let num = length [ () | b <- bs, b ]
      (bs, num, collapse, odds, match)

  let trials = [ testCand cand | cand <- cands ]
  pure $ minimum [ num | (_,num,_,_,match) <- trials, match ]

power :: Int -> [[Bool]]
power n =
  if n < 0 then undefined else
    if n == 0 then [[]] else do
      let bss = power (n-1)
      [ False:bs | bs <- bss ] ++ [ True:bs | bs <- bss ]


data Res = Res { answer :: Int, duration :: Nanos, info :: String }

data Machine = Machine
  { lights :: [Bool]
  , buttons :: [Button]
  , joltage :: [Int]
  }

instance Show Machine where
  show Machine {buttons,joltage} =
    show buttons ++ " -- " ++ show joltage

data Button = Button [Int]

instance Show Button where show (Button xs) = show xs

gram :: Par [Machine]
gram = separated nl machine
  where
    machine = do
      lit '['
      lights <- many light
      lit ']'
      lit ' '
      buttons <- terminated (lit ' ') button
      lit '{'
      joltage <- ints
      lit '}'
      pure Machine { lights, buttons, joltage }

    light = alts [ do lit '.'; pure False
                 , do lit '#'; pure True ]

    button = Button <$> do
      lit '('
      xs <- ints
      lit ')'
      pure xs

    ints = separated (lit ',') int

----------------------------------------------------------------------

timed :: IO a -> IO (a,Nanos)
timed io = do
  before <- getTime Monotonic
  res <- io
  after <- getTime Monotonic
  let TimeSpec{sec,nsec} = after - before
  let time = Nanos (gig * sec + nsec)
  pure ( res, time )

_printTimings :: [(Nanos,String)] -> IO ()
_printTimings xs = do
  putStrLn "\ntimings:"
  forM_ (sortBy (comparing fst) xs) $ \(time,info) -> do
    putStrLn (printf "%s : %s" (show time) info)
  putStrLn $ "\ntotal = " ++ show (sum (map fst xs))

newtype Nanos = Nanos Int64 deriving (Eq,Ord,Num)

instance Show Nanos where
  show (Nanos i) = printf "%s%.03fs" (if dub < 10 then "0" else "") dub
    where dub :: Double = fromIntegral i / fromIntegral gig

gig :: Int64
gig = 1_000_000_000

----------------------------------------------------------------------
-- part2 (new aproach)

part2 :: [(Int,Machine)] -> IO [Res]
part2 ms = do
  printf "\n%d puzzles to solve...\n" (length ms)
  mapM solve2 ms

solve2 :: (Int,Machine) -> IO Res
solve2 (i,m) = do
  let Machine{buttons} = m
  printf "puz#%d : %s \n" i (show buttons)
  let state = makeState m
  (answers,duration) <- timed $ search 2 buttons state
  let answer = minimum answers
  --print ("#answers",answers)
  --print ("answer",answer)
  let info = printf "puz-%03d" i
  pure Res { answer, duration, info }

data State = State { js :: [Int] } deriving (Eq,Ord)

makeState :: Machine -> State
makeState Machine{joltage} = State{js = joltage}

instance Show State where show State {js} = printf "%s" (show js)

done :: State -> Bool
done State{js} = all (==0) js

_printT :: Show a => Int -> a -> IO ()
_printT n a = putStrLn (replicate n ' ' ++ show a)

type Memo = Map State [Int]

search :: Int -> [Button] -> State -> IO [Int]
search tab0 buttons s0 = do
  (answers,_m') <- loop tab0 Map.empty s0
  pure answers
  where
    loop :: Int -> Memo -> State -> IO ([Int],Memo)
    loop tab memo s = do
      --_printT tab s
      if done s then pure ([0],memo) else do
        case Map.lookup s memo of
          Just res -> pure (res,memo)
          Nothing -> do
            (res,memo) <- inner memo (evenUp buttons s)
            let memo' = Map.insert s res memo
            pure (res,memo')

      where
      inner :: Memo -> [(Int,State)] -> IO ([Int],Memo)
      inner memo = \case
        [] -> pure ([],memo)
        (i,s):more -> do
          --_printT tab i
          (as1,memo) <- loop (tab+2) memo (halve s)
          let as1' = [ (i+2*j) | j <- as1 ]
          (as2,memo) <- inner memo more
          let as = as1' ++ as2
          pure (as,memo)

halve :: State -> State
halve State{js} = State { js = [ j `div` 2 | j <- js ] }

evenUp :: [Button] -> State -> [(Int,State)]
evenUp buttons State{js} = do
  let
    testCand bs = do
      let bbs = zip buttons bs
      let m = hist [ x | (Button xs,bool) <- bbs, bool, x<- xs ]
      let look :: Int -> Int = \i -> maybe 0 id $ Map.lookup i m
      let js' = [ j - look i | (i,j) <- zip [0..] js ]
      let match = all (\j -> even j && j >= 0) js'
      let num = length [ () | b <- bs, b ]
      (num, match, js')

  [ (minimum counts,State{js})
    | (js,counts) <-
        collate [ (js',num)
                | bs <- power (length buttons)
                , let (num,match,js') = testCand bs
                , match
                ]
    ]
