module Markov where

import qualified Data.Map as M
import System.Random (StdGen, randomR)


genChain :: (Ord a, Integral i) => [a] -> i -> StdGen -> [a]
genChain list len randGen = reverse $ fst $ foldl addNextElem ([firstElem], randGen') [1..len]
  where probs = genProb list
        (firstElem, randGen') = randomElement (M.keys probs) randGen
        addNextElem ((x:xs), rand) _ = (next:x:xs, rand')
          where (next, rand') = nextElem probs x rand

nextElem :: (Eq a, Ord a) => M.Map a (M.Map a Int) -> a -> StdGen -> (a, StdGen)
nextElem probs prevWord randGen = case mapProbs' of Just a  -> weightedRandom (M.toList a) randGen
                                                    Nothing -> randomElement (M.keys probs) randGen --fallback to random element
  where mapProbs' = M.lookup prevWord probs

randomElement :: [a] -> StdGen -> (a, StdGen)
randomElement list randGen = (list !! randomIndex , randGen')
  where (randomIndex, randGen') = randomR (0,length list - 1) randGen

-- FIXME this is reeeally inefficient for large probabilities: O(sum of all weights)
weightedRandom :: Integral i => [(a,i)] -> StdGen -> (a, StdGen)
weightedRandom list randGen = randomElement weightedList randGen
  where weightedList = concat $ map (\(x,prob) -> replicate (fromIntegral prob) x) list

genProb :: (Eq a, Ord a, Integral i) => [a] -> M.Map a (M.Map a i)
genProb = addProb M.empty

addProb :: (Eq a, Ord a, Integral i) => M.Map a (M.Map a i) -> [a] -> M.Map a (M.Map a i)
addProb p [] = p
addProb p (_:[]) = p
addProb p (x:y:xn) = addProb (M.insertWith (\_ a -> M.insertWith (+) y 1 a) x (M.singleton y 1) p) (y:xn)
