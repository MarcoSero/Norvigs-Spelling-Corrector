import           Data.Char (toLower, isAlpha)
import           Data.List (sortBy)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import           Data.Ord (comparing)

alphabet = "abcdefghijklmnopqrstuvwxyz"
nWords = B.readFile "big.txt" >>= \ws -> return (train (lowerWords (B.unpack ws)))
lowerWords = filter (not . null) . map (map toLower . filter (isAlpha )) . words
train = foldr (\ x acc -> M.insertWith (+) x 1 acc) M.empty

edits1 w = S.fromList $ deletes w ++ transposes w ++ replaces w ++ inserts w
  where splits s = [ splitAt n s | n <- [0..((length s) - 1)] ]
        deletes = (map (\(a, b) -> a ++ (tail b))) . splits
        transposes w = [ a ++ [b !! 1] ++ [b !! 0] ++ (drop 2 b) | (a,b) <- splits w , length b > 1 ]
        replaces w = [a ++ [c] ++ (tail b) | (a,b) <- splits w , c <- alphabet]
        inserts w = [a ++ [c] ++ b | (a,b) <- splits w , c <- alphabet]
edits2 w = S.foldr (S.union) S.empty (S.map edits1 (edits1 w))
knownEdits2 w nwords = (edits2 w) `S.intersection` (M.keysSet nwords)
known inputSet nwords = inputSet `S.intersection` (M.keysSet nwords)

choices w ws = (known (S.singleton w) ws) `orNextIfEmpty` (known (edits1 w) ws)  `orNextIfEmpty` (knownEdits2 w ws) `orNextIfEmpty` (S.singleton w)
  where orNextIfEmpty x y = if S.null x then y else x

chooseBest ch ws = chooseBest' (ws `M.intersection` (M.fromList (map (\x -> (x,0)) (S.toList ch))))
  where chooseBest' bestChs = head $ (map fst (sortCandidates bestChs))
        sortCandidates = sortBy (comparing snd) . M.toList

correct w = nWords >>= \ws -> return (chooseBest (choices w ws) ws)
