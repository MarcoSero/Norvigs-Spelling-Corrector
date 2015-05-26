module Spelling (TrainingDict, nWords, correct) where

import qualified Data.ByteString.Char8            as B
import           Data.Char                        (isAlpha, toLower)
import           Data.List                        (foldl', maximumBy)
import qualified Data.Map.Strict                  as M
import qualified Data.Set                         as S
import           Data.Function                    (on)
import           Paths_Norvigs_Spelling_Corrector (getDataFileName)

type WordSet      = S.Set B.ByteString
type TrainingDict = M.Map B.ByteString Int

alphabet :: String
alphabet = ['a'..'z']

nWords :: IO TrainingDict
nWords = do
  ws <- getDataFileName "big.txt" >>= B.readFile
  return $ (train . lowerWords) ws

lowerWords :: B.ByteString -> [B.ByteString]
lowerWords = B.words . B.map normalize
  where normalize :: Char -> Char
        normalize c = if isAlpha c then toLower c else ' '

train :: [B.ByteString] -> TrainingDict
train = foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

edits1 :: B.ByteString -> WordSet
edits1 w = S.fromList $ deletes ++ transposes ++ replaces ++ inserts
  where splits :: [(B.ByteString, B.ByteString)]
        splits = [ B.splitAt n w | n <- [0 .. B.length w - 1] ]

        deletes :: [B.ByteString]
        deletes = map (\(a, b) -> B.concat[a, B.tail b]) splits

        transposes :: [B.ByteString]
        transposes = [ B.concat [a, B.tail b, b, B.drop 2 b]
                    | (a, b) <- splits ]

        replaces :: [B.ByteString]
        replaces = [ B.concat [a, B.singleton c, B.tail b]
                  | (a, b) <- splits, c <- alphabet]

        inserts :: [B.ByteString]
        inserts = [ B.concat [a, B.singleton c, b]
                  | (a,b) <- splits, c <- alphabet]

edits2 :: B.ByteString -> WordSet
edits2 = S.unions . S.toList . S.map edits1 . edits1

knownEdits2 :: B.ByteString -> TrainingDict -> WordSet
knownEdits2 w nwords = edits2 w `S.intersection` M.keysSet nwords

known :: WordSet -> TrainingDict -> WordSet
known inputSet nwords = inputSet `S.intersection` M.keysSet nwords

choices :: B.ByteString -> TrainingDict -> WordSet
choices w ws = foldr orNextIfEmpty (S.singleton w)
  [ known (S.singleton w) ws
  , known (edits1 w) ws
  , knownEdits2 w ws
  ]
  where orNextIfEmpty x y = if S.null x then y else x

chooseBest :: WordSet -> TrainingDict -> B.ByteString
chooseBest ch ws = maximumBy (compare `on` (\w -> M.findWithDefault 0 w ws)) (S.toList ch)

correct :: TrainingDict -> B.ByteString -> B.ByteString
correct ws w = chooseBest (choices w ws) ws
