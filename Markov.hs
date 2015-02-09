import Data.Char
import System.IO
import System.Environment
import System.Random
import Control.Monad.State
import qualified Data.Map as Map

type MarkovMap = Map.Map String [String]
type MarkovState = ([String], StdGen)
type MarkovSt = State MarkovState String


toSentence :: [String] -> String
toSentence ls = foldl join' "" (reverse ls)
     where join' [] [] = ""
           join' [] (s:ss) = toUpper s : ss
           join' a b = a ++ " " ++ b

showMarkovState :: MarkovState -> String
showMarkovState = toSentence . fst


chain :: String -> [String]
chain line = filter (not . null) $ map clean (words line)
  where clean = ((map toLower) . (filter isAlpha))

insertChain :: [String] -> MarkovMap -> MarkovMap 
insertChain [] mp = mp
insertChain [x] mp = mp
insertChain (x:xs) mp = insertChain xs $ Map.insertWith (++) x [head xs] mp


parseFile :: String -> IO MarkovMap
parseFile fname = openFile fname ReadMode >>= parseFile' Map.empty 
    where parseFile' :: MarkovMap -> Handle -> IO MarkovMap
          parseFile' mp handle = do
            eof <- hIsEOF handle
            if not eof
            then do line <- hGetLine handle
                    parseFile' (insertChain (chain line) mp) handle
            else return mp


addToChain :: MarkovMap -> MarkovState -> (String, MarkovState)
addToChain mp ([], g) = let (idx, ng) = randomR (0, Map.size mp - 1) g
                            start = fst $ Map.elemAt idx mp
                        in (start, ([start], ng))
addToChain mp (ls, g) = let choices = ((Map.!) mp (head ls))
                            (idx, ng) = randomR (0, length choices - 1) g
                            word = choices !! idx
                        in (word, (word:ls, ng))

addToChainSt :: MarkovMap -> MarkovSt
addToChainSt mp = state (addToChain mp)

passChain :: MarkovSt
passChain = do
  ((x:_),g) <- get
  put ([x],g)
  return x

emptyChain :: IO MarkovState
emptyChain = do
  g <- getStdGen
  return ([], g)
                           
main :: IO ()
main = do
  args <- getArgs
  mp <- parseFile (head args)
  start <- emptyChain

  let atc = addToChainSt mp
      ret = snd $ runState (replicateM 12 atc) start
      mrkv = snd $ runState (passChain >> replicateM 12 atc) ret
  putStrLn . showMarkovState $ ret
  putStrLn . showMarkovState $ mrkv
  
