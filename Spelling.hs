module Spelling (TrainingDict, nWords, correct) where

import Paths_Norvigs_Spelling_Corrector (getDataFileName)
import           Data.Char (toLower, isAlpha)
import           Data.List (sortBy, foldl')
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Ord (comparing)

type WordSet = S.Set String
type TrainingDict = M.Map String Int

alphabet :: String
alphabet = ['a' .. 'z']

nWords :: IO TrainingDict
nWords = do
  ws <- getDataFileName "big.txt" >>= B.readFile
  return (train . lowerWords . B.unpack $ ws)

lowerWords :: String -> [String]
lowerWords = words . map normalize
  where normalize c = if isAlpha c then toLower c else ' '

train :: [String] -> TrainingDict
train = foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

edits1 :: String -> WordSet
edits1 w = S.fromList $ deletes ++ transposes ++ replaces ++ inserts
  where
    splits = [ splitAt n w | n <- [0 .. length w - 1] ]
    deletes = map (\(a, b) -> a ++ tail b) splits
    transposes = [ a ++ [b1, b0] ++ bs
                 | (a, b0:b1:bs) <- splits ]
    replaces = [ as ++ [c] ++ bs
               | (as, _:bs) <- splits, c <- alphabet]
    inserts = [ a ++ [c] ++ b
              | (a,b) <- splits, c <- alphabet]

edits2 :: String -> WordSet
edits2 = S.foldl' S.union S.empty . S.map edits1 . edits1

knownEdits2 :: String -> TrainingDict -> WordSet
knownEdits2 w nwords = edits2 w `S.intersection` M.keysSet nwords

known :: WordSet -> TrainingDict -> WordSet
known inputSet nwords = inputSet `S.intersection` M.keysSet nwords

choices :: String -> TrainingDict -> WordSet
choices w ws = foldr orNextIfEmpty (S.singleton w)
  [ known (S.singleton w) ws
  , known (edits1 w) ws
  , knownEdits2 w ws
  ]
  where orNextIfEmpty x y = if S.null x then y else x

chooseBest :: WordSet -> TrainingDict -> String
chooseBest ch ws = chooseBest' $
  ws `M.intersection` M.fromList (map (\x -> (x, ())) (S.toList ch))
  where
    chooseBest' bestChs = head (map fst (sortCandidates bestChs))
    sortCandidates = sortBy (comparing snd) . M.toList

correct :: TrainingDict -> String -> String
correct ws w = chooseBest (choices w ws) ws
