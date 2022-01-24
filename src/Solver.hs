-- | module for parallel components
module Solver where

import Control.Parallel.Strategies (Eval, rpar, runEval)
import Game (possibleGames, score)

-- | If `map` turns a function and a list into a list of
-- thunks, `pmap` turns the same into a list of "sparks";
-- a spark is something that GHC evaluates in parallel,
-- a thunk is something that GHC evaluates later,
-- neither causes the main thread to stop and wait for the result.
--
-- paraphrases the definition of `parMap` from Marlow's book
pmap :: (a -> b) -> [a] -> Eval [b]
pmap f [] = pure []
pmap f (x : xs) = do
  spark <- rpar (f x)
  sparks <- pmap f xs
  return (spark : sparks)

-- | follows the sudoku solver example to
solve :: [String] -> [(String, Int)]
solve ws = runEval $ pmap (score games) ws
  where
    games = possibleGames ws
