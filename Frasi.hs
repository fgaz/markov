import System.IO
import Control.Monad
import qualified Data.Map as M
import System.Random
import qualified Data.Maybe

type Probs = M.Map String Int

genFrase :: String -> StdGen -> Int -> String
genFrase str randGen len = unwords $ reverse $ fst $ foldl addNextWord (["sono"], randGen) [1..len]
  where probs = genProb $ words str
        addNextWord :: ([String], StdGen) -> a -> ([String], StdGen)
        addNextWord ((x:xs), r) _ = ((fst nw):x:xs, snd nw)
          where nw = nextWord probs r x

nextWord :: M.Map String Probs -> StdGen -> String -> (String, StdGen)
--random bilanciato $ elementi lookup
nextWord probs randGen prevWord = weightedRand probs' randGen --TODO se la lunghezza è zero...?
  where probs' = M.toList mapProbs'
        mapProbs' = case M.lookup prevWord probs of Just a -> a
                                                    Nothing -> M.empty


weightedRand :: [(String,Int)] -> StdGen -> (String, StdGen)
weightedRand [] randGen = ("ERRORE", randGen) --FIXME orribile e non generico, correggere l'errore in nextword
weightedRand list randGen = ((weightedList !! r), randGen')
  where weightedList = concat $ map (\(x,prob) -> replicate prob x) list
        (r, randGen') = randomR (0,length list - 1) randGen


genProb = addProb M.empty

addProb :: M.Map String Probs -> [String] -> M.Map String Probs
addProb p [] = p
addProb p (_:[]) = p
addProb p (x:y:xn) = addProb (M.insertWith (\_ a -> M.insertWith (+) y 1 a) x (M.singleton y 1) p) (y:xn)


main = do
    handle <- openFile "i_promessi_sposi.txt" ReadMode
    contents <- hGetContents handle
    randGen <- getStdGen
    putStrLn "Lunghezza frase?"
    --len <- readLn
    let len = "300"
    putStrLn $ genFrase contents randGen $ read len