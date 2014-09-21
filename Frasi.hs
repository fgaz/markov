import System.IO
import qualified Data.Map as M
import System.Random
import System.Environment
import System.Exit

type Probs = M.Map String Int

genFrase :: String -> Int -> StdGen -> String
genFrase str len randGen = unwords $ reverse $ fst $ foldl addNextWord ([firstWord], randGen) [1..len]
  where probs = genProb $ words str
        firstWord = fst $ randomElement (M.keys probs) randGen --FIXME non aggiorna randGen
        addNextWord :: ([String], StdGen) -> a -> ([String], StdGen)
        addNextWord ((x:xs), randGen) _ = (next:x:xs, randGen')
          where (next, randGen') = nextWord probs x randGen

nextWord :: M.Map String Probs -> String -> StdGen -> (String, StdGen)
nextWord probs prevWord randGen = case mapProbs' of Just a  -> weightedRandom (M.toList a) randGen
                                                    Nothing -> randomElement (M.keys probs) randGen --fallback to random word
  where mapProbs' = M.lookup prevWord probs

randomElement :: [a] -> StdGen -> (a, StdGen)
randomElement list randGen = (list !! randomIndex , randGen')
  where (randomIndex, randGen') = randomR (0,length list - 1) randGen

weightedRandom :: [(a,Int)] -> StdGen -> (a, StdGen)
weightedRandom list randGen = randomElement weightedList randGen
  where weightedList = concat $ map (\(x,prob) -> replicate prob x) list


genProb = addProb M.empty

addProb :: M.Map String Probs -> [String] -> M.Map String Probs
addProb p [] = p
addProb p (_:[]) = p
addProb p (x:y:xn) = addProb (M.insertWith (\_ a -> M.insertWith (+) y 1 a) x (M.singleton y 1) p) (y:xn)


main = do
    args <- getArgs
    if length args < 2 then do
        putStrLn "Usage: frasi [number of words to generate] [file to analyze]"
        exitFailure
    else return ()
    handle <- openFile (args !! 1) ReadMode
    contents <- hGetContents handle
    randGen <- getStdGen
    let len = read $ head args
    putStrLn $ genFrase contents len randGen