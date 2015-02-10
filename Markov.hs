import Data.Char
import Data.Maybe
import System.IO
import System.Random
import System.Environment
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map


type MarkovMap = Map.Map String [String]
type MarkovState = ([String], StdGen)
type MarkovSt = State MarkovState String


-- Turns a line of text into a list of strings containing only lowercase letters
toChain :: String -> [String]
toChain line = filter (not . null) $ map clean (words line)
  where clean = ((map toLower) . (filter isAlpha))


-- Adds a new word to the chain mapped to by the given key
markovMapInsert :: MarkovMap -> String -> String -> MarkovMap
markovMapInsert mp _ [] = mp
markovMapInsert mp k v = if k == (head $ words v) then mp
                         else Map.insertWith (++) k [v] mp


-- Maps every word in a chain to a list of words that have followed it
insertChain :: [String] -> MarkovMap -> MarkovMap 
insertChain [] mp = mp
insertChain [_] mp = mp
insertChain [k,v] mp = markovMapInsert mp k v
insertChain (a:as) mp = -- insertChain as $ markovMapInsert mp a (head as)
    let b = head as
        c = head . tail $ as
        val = if (length b + length c) < 8
              then b ++ " " ++ c
              else b
    in insertChain as $ markovMapInsert mp a val


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
addToChain mp ([], g) =
    let (idx, ng) = randomR (0, Map.size mp - 1) g
        key = fst $ Map.elemAt idx mp
    in (key, ([key], ng))
addToChain mp (ls, g) =
    let mbChoices = Map.lookup (head ls) mp 
    in if isJust mbChoices
       then let Just choices = mbChoices
                (idx,ng) = randomR (0, length choices - 1) g
                val =  choices !! idx
            -- in (val, (val:ls, ng))
            in if ' ' `elem` val
               then let l = foldl (flip (:)) ls (words val)
                    in (head l, (l, ng))
               else (val, (val:ls, ng))
       else addToChain mp ([], g)


-- Wraps the addToChain function in a Stateful Computation, so you can do
-- Monadic things with it!
addToChainSt :: MarkovMap -> MarkovSt
addToChainSt = state . addToChain


-- A Markov chain with no words and a new random number generator
initMarkovState :: IO MarkovState
initMarkovState = do
  g <- getStdGen
  return ([], g)


-- Sets a markov chain to the initial empty state
reInitChain :: MarkovSt
reInitChain = do
  (_,g) <- get
  put ([], snd (next g))
  return ""


-- Sets the markov chain to contain only the first word of the current chain
resetChain :: MarkovSt
resetChain = do
  (ws,g) <- get
  if not $ null ws
  then let w = last ws
           ng = snd $ next g
       in put ([w],ng) >> return w
  else reInitChain
         

-- Sets the markov chain to only contain the last word of the current chain
passChain :: MarkovSt
passChain = do
  (ws,g) <- get
  if not $ null ws
  then let w = head ws
           ng = snd $ next g
       in put ([w],ng) >> return w
  else reInitChain


-- Resets the markov chain and repeats a state manipulation `num` times
repeatMarkovSt :: Int -> MarkovSt -> MarkovSt
repeatMarkovSt num st = do
    ls <- replicateM num st
    return $ head ls


-- Like runState but only returns the state, not the result
runMarkovSt :: MarkovSt -> MarkovState -> MarkovState
runMarkovSt st start = snd $ runState st start


-- Converts the state of a Markov chain to a readable sentence
showMarkovState :: MarkovState -> String
showMarkovState = toSentence . fst
    where toSentence ls = foldl join' "" (reverse ls)
              where join' [] [] = ""
                    join' [] (s:ss) = toUpper s : ss
                    join' a b = a ++ " " ++ b


main :: IO ()
main = do
  args <- getArgs
  maps <- mapM parseFile args
  empty <- initMarkovState

  let bigmap = foldl (Map.unionWith (++)) Map.empty maps
      append = addToChainSt bigmap
      seven start st = runMarkovSt (passChain >> repeatMarkovSt 7 st) start
      states = scanl seven empty (repeat append)

  mapM_ (putStrLn . showMarkovState) $ take 100 states
