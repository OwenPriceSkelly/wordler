-- | responsible for the wordle clone's game logic
module Game where

import Control.Monad (guard)
import Data.Char ( Char, toLower, toUpper )
import Data.List (sortOn)
import System.Random ( randomRIO )
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
  if word `elem` wordbank gs
    then gs {guessed = zipWith3 f (solution gs) word (guessed gs)}
    else gs
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

-- | "guesses" the given word for each game in games and sums the respective
-- post-guess wordbank sizes. This might not end up being the best measure but
-- seems like a reasonable place to start.
score :: String -> [GameState] -> Int
score word = sum . map (length . wordbank . prune . guess word)
