{-# LANGUAGE UnicodeSyntax #-}
module Language.Guess where

import Control.Applicative ((<$>))
import Data.Char
import Data.Function (on)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Tuple
import Data.List (sort, sortBy)

import qualified Data.ByteString.Char8 as BS
import Data.Serialize

import Paths_language_guess

type Trigram   = (Char, Char, Char)
type Frequency = Int
type Rank      = Int
type Language  = String

threshold ∷ Int
threshold = 300

-- | Load a cerealized file.
loadData ∷ FilePath → IO (Map Language (Map Trigram Rank))
loadData f = (\(Right x) → x) . decode <$> BS.readFile f

-- | Load the default cerealized file.
loadData' ∷ IO (Map Language (Map Trigram Rank))
loadData' = loadData =<< getDataFileName "lang.dat"

-- | Guess the language of a string.
guess ∷ Map Language (Map Trigram Rank) → String → [(Language, Double)]
guess langData = sortBy (flip compare `on` swap) . M.toList . f . rank . parse
  where f x = M.map (flip distance x) langData

-- | Calculate distance between ranked trigram sets.
-- Cavnar & Trenkle (1994)
distance ∷ Map Trigram Rank → Map Trigram Rank → Double
distance x y = norm $ M.foldrWithKey f 0 y
  where f k n m = m + fromMaybe threshold (abs . (n-) <$> M.lookup k x)
        norm z = 1 - fromIntegral z / fromIntegral (M.size y)
                                    / fromIntegral (M.size x)

-- | Convert a set of trigram frequencies to ranks.
-- Maximum of 'threshold', uses alphabetical sort to break ties.
rank ∷ Map Trigram Frequency → M.Map Trigram Rank
rank = M.fromList . flip zip [1..] . map snd . take threshold . sortBy c
                  . map swap . M.toList
  where (r, k) `c` (r', k') = if r == r' then compare k k'
                                         else compare r' r

-- | Make a trigram frequency map out of a string.
parse ∷ String → Map (Char, Char, Char) Frequency
parse x = go M.empty $ clean (' ':x)
  where go m (x:y:z:xs) = go (M.alter f (x,y,z) m) (y:z:xs)
        go m _ = m
        f Nothing = Just 1
        f (Just a) = Just (a+1)

-- | Clean a string, removing punctiation, lowering cases, and collapsing
-- adjacent spaces.
clean ∷ String → String
clean (x:y:xs)
    | isWhite x && isWhite y = clean (' ':xs)
    | isPunctuation y || isNumber y = clean (x:' ':xs)
    | isUpper y = clean (x:toLower y:xs)
    | otherwise = x:clean (y:xs)
clean (' ':[]) = " "
clean (x:[]) = x:" "
clean _ = ""

-- | If a character is a whitespace.
isWhite ∷ Char → Bool
isWhite x = isSpace x || isSeparator x
