import System.IO (openFile, IOMode (ReadMode), hGetContents)
import System.Random (getStdGen)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Markov (genChain)


main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then do
        putStrLn "Usage: frasi [number of words to generate] [file to analyze]"
        exitFailure
    else return ()
    handle <- openFile (args !! 1) ReadMode
    contents <- hGetContents handle
    randGen <- getStdGen
    let len = (read $ head args) :: Integer
    putStrLn $ unwords $ genChain (words contents) len randGen
