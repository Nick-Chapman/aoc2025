module Day10 (main) where

import Misc (check,hist) --nub
import Par4 (parse,Par,many,terminated,separated,alts,int,lit,nl)
import Data.Map qualified as Map
import Data.List (intercalate)
import Text.Printf (printf)
import Data.List (minimumBy)
import Data.Ord (comparing)


import Control.Monad (forM_)
import Data.List (sortBy)
--import Data.Ord (comparing)
import GHC.Int (Int64)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
--import Text.Printf (printf)


main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day10.sample"
  inp <- parse gram <$> readFile "input/day10.input"
  part1_sam <- part1 sam
  print ("day10, part1 (sample-details)", check [2,3,2] $ part1_sam)
  print ("day10, part1 (sample)", check 7 $ sum $ part1_sam)
  part1_inp <- part1 inp
  print ("day10, part1", check 461 $ sum $ part1_inp)

  sam_details <- part2 (zip [1000..] sam)
  let sam_xs = [ answer | Res{answer} <- sam_details ]
  print ("day10, part1 (sample)", check [10,12,11] $ sam_xs)

  let expected = [128,42,80,9,75,101,80,32,101,219,91,186,59,54,243,90,138,245,64,127,50,59,88,34,160,163,108,101,69,127,69,57,233,52,47,51,49,64,44,52,69,215,307,249,106,185,84,95,88,38,186,11,65,132,104,64,140,200,62,25,66,93,41,82,55,74,62,108,79,105,77,36,52,65,93,50,89,41,195,177,96,116,63,211,68,85,223,56,58,47,40,7,80,30,74,103,52,64,19,138,98,26,196,53,65,112,60,114,94,48,54,88,233,159,79,58,66,283,135,103,114,111,37,127,257,49,70,103,36,26,88,145,39,107,80,76,161,100,171,48,53,215,64,244,80,96,62,102,24,123,105,57,65,70,72,64,89,83,104,50,177,59,229,62,95,58,279]

  let _hard = [118,141,67,99,10,26,119,131,164]
  let _easy = [ i | i <- [0..166], i `notElem` _hard ]

  let pick = _easy

  let iPicked = [ (i,x) | (i,x) <- zip [0::Int ..] inp, i `elem` pick ]
  let xPicked = [ x | (i,x) <- zip [0::Int ..] expected, i `elem` pick ]

  details <- part2 iPicked
  let xs = [ answer | Res{answer} <- details ]

  --_printTimings [ (duration,info) | Res{duration,info} <- details ]

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

part2 :: [(Int,Machine)] -> IO [Res]
part2 ms = do
  printf "\n%d puzzles to solve...\n" (length ms)
  mapM solve2 ms --(zip [0..] ms)

solve2 :: (Int,Machine) -> IO Res
solve2 (i,m) = do
  printf "puz#%d: %s\n" i (show m)
  let state = makeState m
  --if i `elem` [10,26,53,99,119,131,164] then print state else pure ()
  (ss,duration) <- timed $ search state
  let answer = minimum [ sum [ i | (_,i) <- ass ] | State{ass} <- ss ]
  --print ("#ss=",length ss,"res=",res)
  let info = printf "puz-%03d; #sol = %d" i (length ss)
  pure Res { answer, duration, info }

search :: State -> IO [State]
search s0 = do
  --print s0
  prop s0 >>= \case
    Nothing -> pure []
    Just s1 -> do
      if (finished s1) then pure [s1] else do
        let (x,max) = pickVar s1
        --print ("pick",x,max)
        concat <$> sequence [ search s3 | i <- [0..max], Just s3 <- pure (assignVar  s1 x i) ]

prop :: State -> IO (Maybe State)
prop s =
  case findUnit s of
    Nothing -> pure (Just s)
    Just (x,i) -> do
      case assignVar s x i of
        Nothing -> pure Nothing
        Just s -> prop s

findUnit :: State -> Maybe (Char,Int)
findUnit State{eqs} = do
  case [ (c,n) | Equation [c] n <- eqs ] of
    [] -> Nothing
    pair:_ -> Just pair

data State = State { ass :: [(Char,Int)], eqs :: [Equation] }
data Equation = Equation [Var] Int deriving (Eq,Ord)
type Var = Char

instance Show Equation where
  show (Equation bs x) = intercalate "+" [ [b] | b <- bs] ++ "=" ++ show x

instance Show State where
  show State {ass,eqs} =
    printf "%s :\n%s" (show ass) (intercalate "\n" (map show eqs))

makeState :: Machine -> State
makeState Machine{buttons,joltage} = do
  let labelled = zip ['a'..] buttons
  let
    pick :: Int -> [Char]
    pick i = [ c | (c,Button ns) <- labelled, i `elem` ns ]
  let eqs = [ Equation (pick i) j | (i,j) <- zip [0..] joltage ]
  State { eqs, ass = []}

finished :: State -> Bool
finished State{eqs} = length eqs == 0

pickVar :: State -> (Var,Int)
pickVar s@State{eqs} = do
  let Equation xs _n = minimumBy (comparing smallerE) eqs
  case xs of
    [] -> error (show ("pickVar",s))
    x:_ -> do
      let n = minimum [ n | Equation xs n <- eqs, x `elem` xs ]
      (x,n)
    where
      smallerE (Equation xs _n) = length xs
      --smallerE (Equation xs n) = (length xs,n)

assignVar :: State -> Var -> Int -> Maybe State
assignVar State {ass,eqs} x n = do
  let ass' = (x,n) : ass
  case allJust [ assignVarE x n e | e <- eqs ] of
    Nothing -> Nothing
    Just eqs -> do
      let eqs' = [ e | e@(Equation (_:_) _) <- eqs ]
      Just $ State {ass = ass', eqs = eqs'}

assignVarE :: Var -> Int -> Equation -> Maybe Equation
assignVarE x i e@(Equation xs n) =
  if x `elem` xs
  then mkEquation (filter (/=x) xs) (n-i)
  else Just e

mkEquation :: [Var] -> Int -> Maybe Equation
mkEquation xs n =
  if (n < 0 || case xs of [] -> n > 0; _:_ -> False) then Nothing else
    Just (Equation xs n)

allJust :: [Maybe a] -> Maybe [a]
allJust = traverse id

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
