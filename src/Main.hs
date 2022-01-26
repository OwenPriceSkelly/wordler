module Main where

import Data.List (sortOn)
import Game (possibleGames, score)
import Solver (solve, solve')

main :: IO ()
main = do
  putStrLn "hello wordle!"
  wordbank <- lines <$> readFile "words.txt"
  let scores = solve' wordbank
      results = sortOn snd scores
  writeFile "results.txt" $ unlines [show s | s <- results]
  putStrLn "wrote to results.txt."
  putStrLn "goodbye wordle!"
