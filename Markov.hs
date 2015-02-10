import Data.Char
import System.IO
import System.Environment
import System.Random
import Control.Monad.State
import qualified Data.Map as Map

type MarkovMap = Map.Map String [String]
type MarkovState = ([String], StdGen)
type MarkovSt = State MarkovState String


-- Turns a line of text into a list of strings containing only lowercase letters
toChain :: String -> [String]
toChain line = filter (not . null) $ map clean (words line)
  where clean = ((map toLower) . (filter isAlpha))


-- Maps every word in a chain to a list of words that have followed it
insertChain :: [String] -> MarkovMap -> MarkovMap 
insertChain [] mp = mp
insertChain [x] mp = mp
insertChain (x:xs) mp = insertChain xs $ Map.insertWith (++) x [head xs] mp


-- Converts a line of text to a chain moves the words into a map
insertLine :: String -> MarkovMap -> MarkovMap
insertLine = insertChain . toChain


-- Inserts every line of an open file into a map
markovMapFile :: MarkovMap -> Handle -> IO MarkovMap
markovMapFile mp file = do
  eof <- hIsEOF file
  if not eof
  then do line <- hGetLine file
          markovMapFile (insertLine line mp) file
  else return mp


-- Opens the given filename and inserts the lines of the file into a new map
parseFile :: String -> IO MarkovMap
parseFile fname = openFile fname ReadMode >>= markovMapFile Map.empty 


-- Takes a Markov chain, randomly chooses a word which might follow the current
-- word, and returns that word coupled with the new state of the markov chain
addToChain :: MarkovMap -> MarkovState -> (String, MarkovState)
addToChain mp ([], g) = let (idx, ng) = randomR (0, Map.size mp - 1) g
                            start = fst $ Map.elemAt idx mp
                        in (start, ([start], ng))
addToChain mp (ls, g) = let choices = ((Map.!) mp (head ls))
                            (idx, ng) = randomR (0, length choices - 1) g
                            word = choices !! idx
                        in (word, (word:ls, ng))


-- Wraps the addToChain function in a Stateful Computation, so you can do
-- Monadic things with it!
addToChainSt :: MarkovMap -> MarkovSt
addToChainSt = state . addToChain


-- A Markov chain with no words and a new random number generator
initMarkovState :: IO MarkovState
initMarkovState = do
  g <- getStdGen
  return ([], g)


-- Converts the state of a Markov chain to a readable sentence
showMarkovState :: MarkovState -> String
showMarkovState = toSentence . fst
    where toSentence ls = foldl join' "" (reverse ls)
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


-- Like runState but only returns the state, not the result
runMarkovSt :: MarkovSt -> MarkovState -> MarkovState
runMarkovSt st start = snd $ runState st start


main :: IO ()
main = do
  args <- getArgs
  maps <- mapM parseFile args
  empty <- initMarkovState

  let bigmap = foldl (Map.unionWith (++)) Map.empty maps
      append = addToChainSt bigmap
      eight start st = runMarkovSt (repeatMarkovSt 8 st) start
      states = scanl eight empty (replicate 10 append)

  mapM_ (putStrLn . showMarkovState) states
