module Main where

import Data.List (sortOn)
import Game (possibleGames, score)

main :: IO ()
main = do
  putStrLn "hello wordle!"
  wordbank <- lines <$> readFile "words.txt"
  let games = possibleGames wordbank
      scores = [(w, score w games) | w <- wordbank]
      results = sortOn snd scores
  writeFile "results.txt" $ unlines [show s | s <- results]
  putStrLn "wrote to results.txt.\ngoodbye wordle!"
