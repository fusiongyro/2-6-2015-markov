import Data.Char
import System.IO
import System.Random
import System.Environment
import Control.Monad
import Control.Monad.State
import qualified Data.Map as Map


type MarkovMap = Map.Map String [String]
type MarkovState = ([String], StdGen)
type MarkovSt = State MarkovState String


-- \_ >>> :t (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- \_ >>> :t (>>= (const mst))
-- (>>= (const mst)) :: (MarkovState -> a) -> MarkovState -> (String, MarkovState)
-- \_ >>> :t (mst >>=)
-- (mst >>=) :: ((String, MarkovState) -> MarkovState -> b) -> MarkovState -> b
                            

-- Turns a line of text into a list of strings containing only lowercase letters
toChain :: String -> [String]
toChain line = filter (not . null) $ map clean (words line)
  where clean = ((map toLower) . (filter isAlpha))


-- Adds a new word to the chain mapped to by the given key
markovMapInsert :: MarkovMap -> String -> String -> MarkovMap
markovMapInsert mp _ [] = mp
markovMapInsert mp k v = if k == (head $ words v) then mp
                         else Map.insertWith' (++) k [v] mp


-- Maps every word in a chain to a list of words that have followed it
insertChain :: [String] -> MarkovMap -> MarkovMap 
insertChain [] mp = mp
insertChain [_] mp = mp
insertChain [k,v] mp = markovMapInsert mp k v
insertChain (a:as) mp = let shorts = takeWhile ((< 4) . length) as
                            val = if null shorts then head as
                                  else tail $ concatMap (' ' :) shorts
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
addToChain mp ([], g) = (val, ([val], ng))
    where (idx, ng) = randomR (0, Map.size mp - 1) g
          val = fst $ Map.elemAt idx mp
addToChain mp (ws, g) =
    case Map.lookup (head ws) mp of
      Nothing -> addToChain mp ([], g)
      Just steps -> if ' ' `elem` val
                    -- then let chain = (reverse $ words val) ++ ws
                    then let chain = foldl (flip (:)) ws $ words val
                             ch = head chain
                         in (ch, (chain, ng))
                    else (val, (val:ws, ng))
          where (idx, ng) = randomR (0, length steps - 1) g
                val = steps !! idx


-- Wraps the addToChain function in a Stateful Computation, so you can do
-- Monad things with it!
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
repeatMarkovSt num action = do
    ls <- replicateM num action
    return $ head ls


-- Adds a given number of words from a map to a markov chain
addWordsToChain :: MarkovMap -> Int -> MarkovSt
addWordsToChain mp n = repeatMarkovSt n (addToChainSt mp)


-- Like runState but only returns the state, not the result
runMarkovSt :: MarkovState -> MarkovSt -> MarkovState
runMarkovSt start action = snd $ runState action start


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

  let mrkvMap = Map.unionsWith (++) maps
      -- I can create an infinite list of stateful computations to shape the
      -- output of the program to my liking. For example, this writes short
      -- "poetic" four-line stanzas, such that the last word of each line is the
      -- first word of the next line!
      machine = zipWith (>>) (cycle $ reInitChain : (replicate 3 passChain)) $
                repeat (addWordsToChain mrkvMap 7)
      -- When I scan over that list computations I can create an infinite list
      -- of snapshots of the state of the markov chain... INFINITE BAD POETRY!
      states = tail $ scanl runMarkovSt empty $ take 24 machine

  mapM_ (putStrLn . showMarkovState) states
