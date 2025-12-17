module Day12 (main) where

import Misc (check)
import Par4 (parse,Par,separated,sp,nl,lit,int,dot)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day12.input"
  part1_inp <- part1 inp
  print ("day12, part1", check 408 $ part1_inp)

part1 :: [Puz] -> IO Int
part1 ps = do
  let _view = mapM_ print [ (p,info p) | p <- ps ]
  let numFit = length [ () | p <- ps, let Info{slack} = info p, not (slack < 0) ]
  pure numFit

info :: Puz -> Info
info Puz{dims=(w,h),nums} = do
  let bs = [ 7,5,6,7,7,7 ]
  let area = w * h
  let perim = 2 * (w * h)
  let pips = sum (zipWith (*) bs nums)
  let slack = area - pips
  Info {area,perim,pips,slack}

data Info = Info
  { area :: Int
  , perim :: Int
  , pips :: Int
  , slack :: Int
  } deriving Show

data Puz = Puz
  { dims :: (Int,Int)
  , nums :: [Int]
  }
  deriving Show

gram :: Par [Puz]
gram = do
  sequence_ (replicate 6 block)
  separated nl puz

  where
    block = do
      x;x;nl
      x;x;x;nl
      x;x;x;nl
      x;x;x;nl
      nl
      pure (error "ignore block")

    x = do _ <- dot; pure ()

    puz = do
      h <- int
      lit 'x'
      w <- int
      lit ':'
      sp
      nums <- separated sp int
      pure Puz{ dims = (w,h), nums }
