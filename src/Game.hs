{-# LANGUAGE LambdaCase #-}

-- | responsible for the wordle clone's game logic
module Game where

import Control.Applicative (liftA2)
import Control.Monad (guard)
import Data.Char (Char, toLower, toUpper)
import Data.List (sortOn)
import System.Random (randomRIO)
import Prelude hiding (Right)

data GameState = GameState
  { solution :: String,
    wordbank :: [String],
    guessed :: [Cell]
  }

instance Show GameState where
  show gs =
    -- solution gs ++
    "\n" ++ concat (show <$> guessed gs)

data Cell = Right Char | Wrong Char | Misplaced Char
  deriving (Eq)

instance Show Cell where
  show (Right c) = [toUpper c]
  show (Wrong _) = "_"
  show (Misplaced c) = [toLower c]

instance Ord Cell where
  _ <= (Right _) = True
  (Wrong _) <= _ = True
  _ <= _ = False

-- | Choose a word at random from file and yield a new gamestate
randomGameState :: IO GameState
randomGameState = do
  wordbank <- lines <$> readFile "words.txt"
  idx <- randomRIO (0, length wordbank - 1)
  let solution = wordbank !! idx
      guessed = Wrong '_' <$ [1 .. 5]
  return $ GameState solution wordbank guessed

-- | update GameState per the guessed word and gamestate's solution
guess :: String -> GameState -> GameState
guess word gs =
  gs {guessed = zipWith3 f (solution gs) word (guessed gs)}
  where
    f s g (Right c) = Right c
    f s g _
      | s == g = Right g
      | g `elem` solution gs = Misplaced g
      | otherwise = Wrong g

-- | Update gamestate s.t. only plausible solutions remain in wordbank.
--
-- this is kind of a roundabout way of doing it; I'm hoping that I've built
-- the composite filter `f` in a way that is amenable to the list fusion rules,
-- but (on the off chance that GHC can't read my mind), it's probably worth
-- comparing a few different approaches.
--
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/rewrite_rules.html#list-fusion
prune :: GameState -> GameState
prune gs =
  let cells = sortOn snd . zip [0 ..] . guessed $ gs -- sort s.t. we filter by `Right` cells first
      filters = filter . mkPred <$> cells
      f = foldr (.) id filters
   in gs {wordbank = f (wordbank gs)}
  where
    mkPred (idx, Right ch) = \w -> (w !! idx) == ch
    mkPred (_, Misplaced ch) = \w -> ch `elem` w
    mkPred (_, Wrong ch) = \w -> ch `notElem` w

-- | builds the list of all possible games from a given wordbank
possibleGames :: [String] -> [GameState]
possibleGames ws = [GameState s ws emptyGuess | s <- ws]
  where
    emptyGuess = Wrong ' ' <$ [1 .. 5]

-- | "guesses" the given word for each game and sums the respective
-- post-guess wordbank sizes.
score :: [GameState] -> String -> (String, Int)
score games word = (word, sum $ map f games)
  where
    f = length . wordbank . prune . guess word

score' :: [String] -> String -> (String, Int)
score' wordbank guess = (guess, length $ prune' wordbank guess)

-- | rewritten prune that just operates on the wordbank directly and composes
-- the predicates in a way that is (maybe) a little more efficient.
prune' :: [String] -> String -> [String]
prune' wordbank guess = do
  sol <- wordbank
  let cs = sortOn snd $ zip [0 ..] (cells guess sol)
      preds = mkPred <$> cs
      plausible = and . sequence preds -- nts: this works because (-> b) is a monad instance
  guard (plausible sol)
  return sol
  where
    (<&&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
    (<&&>) = liftA2 (&&)

    mkPred :: (Int, Cell) -> String -> Bool
    mkPred (idx, Right ch) = \w -> (w !! idx) == ch
    mkPred (_, Misplaced ch) = \w -> ch `elem` w
    mkPred (_, Wrong ch) = \w -> ch `notElem` w


-- let cs = cells guess <$> wordbank in wordbank

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

isDud :: [Cell] -> Bool
isDud = all (\case Wrong _ -> True; _ -> False)
