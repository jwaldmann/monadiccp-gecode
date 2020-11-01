{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

import System.Environment
import Control.CP.FD.GecodeExample
import Control.Monad (forM, when)

-- cf. https://gitlab.imn.htwk-leipzig.de/waldmann/computer-mu/-/blob/master/vuza/Canon.hs

model :: ExampleModel ()
model () = do
  let n = 72
  existsRangeN (0,n-1) 6 $ \xs -> do
    monotone xs
    aperiodic n xs
    existsRangeN (0,n-1) 12 $ \ys -> do
      monotone ys
      aperiodic n ys
      allDiff $ list $ do
        x <- xs ; y <- ys
        return $ mod (x + y) n
      return $ list $ xs <> ys

-- forall i : exists j :  x[i] - x[0] /= x[i+j] - x[0+j]  (mod n)
aperiodic n xs = 
  let w = cte $ length xs
      x = list xs
  in  loopall (1,w-1) $ \ i -> 
        let d = x ! i - x ! 0
        in  inv $ loopall (1,w-1) $ \ j -> 
            let e = x ! mod (i+j) w - x ! j
            in  d @= mod (e + n) n

modS x n = (x @>= n) @? (x - n, x)

monotone xs =
  -- forM (zip xs $ tail xs) $ \ (x,y) -> x @< y
  sSorted $ list xs

existsRangeN bnd 0 k = k []
existsRangeN (lo,hi) n k = exists $ \ x -> do
  cte lo @<= x ; x @<= cte hi
  existsRangeN (lo,hi) (n-1) $ \ xs -> k (x:xs)

main = getArgs >>= \ case
  [] -> withArgs ["gecode_run"] $ example_sat_main_void_gecode model
  _  -> example_sat_main_void_gecode model

