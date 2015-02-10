import Data.Char
import System.IO
import System.Environment
import System.Random
import Control.Monad.State
import qualified Data.Map as Map

type MarkovMap = Map.Map String [String]
type MarkovState = ([String], StdGen)
type MarkovSt = State MarkovState String


toChain :: String -> [String]
toChain line = filter (not . null) $ map clean (words line)
  where clean = ((map toLower) . (filter isAlpha))


insertChain :: [String] -> MarkovMap -> MarkovMap 
insertChain [] mp = mp
insertChain [x] mp = mp
insertChain (x:xs) mp = insertChain xs $ Map.insertWith (++) x [head xs] mp


-- insertLine :: String -> MarkovMap -> MarkovMap
-- insertLine line = insertChain (toChain line)


markovMapFile :: MarkovMap -> Handle -> IO MarkovMap
markovMapFile mp handle = do
  eof <- hIsEOF handle
  if not eof
  then do line <- hGetLine handle
          markovMapFile (insertChain (toChain line) mp) handle
  else return mp


parseFile :: String -> IO MarkovMap
parseFile fname = openFile fname ReadMode >>= markovMapFile Map.empty 


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


initMarkovState :: IO MarkovState
initMarkovState = do
  g <- getStdGen
  return ([], g)


showMarkovState :: MarkovState -> String
showMarkovState = toSentence . fst
    where -- toSentence :: [String] -> String
          toSentence ls = foldl join' "" (reverse ls)
              where join' [] [] = ""
                    join' [] (s:ss) = toUpper s : ss
                    join' a b = a ++ " " ++ b


-- Sets the markov chain to only contain the last word of the current chain
passChain :: MarkovSt
passChain = do
  (ws,g) <- get
  if not $ null ws
  then let w = head ws
       in put ([w],g) >> return w
  else return ""


-- Resets the markov chain and repeats a state manipulation `num` times
repeatMarkovSt :: Int -> MarkovSt -> MarkovSt
repeatMarkovSt num st = do
    passChain
    ls <- replicateM num st
    return $ head ls


runMarkovState :: MarkovSt -> MarkovState -> MarkovState
runMarkovState st start = snd $ runState st start


passState :: Int -> MarkovState -> MarkovSt -> MarkovState
passState len start st = runMarkovState (repeatMarkovSt len st) start


main :: IO ()
main = do
  args <- getArgs
  maps <- mapM parseFile args
  e <- initMarkovState

  let bigmap = foldl (Map.unionWith (++)) Map.empty maps
      atc = addToChainSt bigmap
      states = scanl (passState 12) e (replicate 5 atc)

  mapM_ (putStrLn . showMarkovState) states
  
