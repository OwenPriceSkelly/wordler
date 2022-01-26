-- | module for parallel components
module Solver where

import Control.Parallel.Strategies (Eval, rpar, runEval)
import Game (possibleGames, score, Cell(..))
import Prelude hiding (Right)

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

-- | follows the sudoku solver example
solve :: [String] -> [(String, Int)]
solve ws = runEval $ pmap (score games) ws
  where
    games = possibleGames ws

solve' :: [String] -> [String] -> [(String, Int)]
solve' solutions wordbank = runEval $ pmap f wordbank
  where f guess = (guess, score' solutions guess)

-- | `score'` works directly on the wordbank rather than a gamestate, and
-- simplifies things by just recording the number of plausible words
-- across all possible solutions
score' :: [String] -> String -> Int
score' solutions guess = sum . map length $
  do
    sol <- solutions
    let cs = zip [0 ..] (cells guess sol)
        preds = mkPred <$> cs
        plausible = and . sequence preds -- nts: this works because (-> b) is a monad
    pure (filter plausible solutions)

mkPred :: (Int, Cell) -> String -> Bool
mkPred (idx, Right ch) = \w -> (w !! idx) == ch
mkPred (_, Misplaced ch) = \w -> ch `elem` w
mkPred (_, Wrong ch) = \w -> ch `notElem` w

-- | builds the hypothetical 5-cell "signature" that you'd get by guessing
-- `guess` in a game that had solution `sol`
cells :: String -> String -> [Cell]
cells guess sol = zipWith f sol guess
  where
    f :: Char -> Char -> Cell
    f s c
      | s == c = Right c
      | c `elem` sol = Misplaced c
      | otherwise = Wrong c
