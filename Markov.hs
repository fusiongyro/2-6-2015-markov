import Data.Char
import System.IO
import System.Environment
import System.Random
import Control.Monad.State
import qualified Data.Map as Map

type MarkovMap = Map.Map String [String]

chain :: String -> [String]
chain line = filter (not . null) $ map clean (words line)
  where clean = ((map toLower) . (filter isAlpha))

insertChain :: [String] -> MarkovMap -> MarkovMap 
insertChain [] mp = mp
insertChain [x] mp = mp
insertChain (x:xs) mp = insertChain xs $ Map.insertWith (++) x [head xs] mp

parseFile' :: MarkovMap -> Handle -> IO MarkovMap
parseFile' mp handle = do
  eof <- hIsEOF handle
  if not eof
    then do line <- hGetLine handle
            parseFile' (insertChain (chain line) mp) handle
    else return mp

parseFile :: String -> IO MarkovMap
parseFile fname = openFile fname ReadMode >>= parseFile' Map.empty 

randomRSt :: Int -> State StdGen Int
randomRSt num = state (randomR (0, num - 1))

randomIdxSt :: [a] -> State StdGen Int
randomIdxSt ls = randomRSt $ length ls

main :: IO ()
main = do
  args <- getArgs
  mp <- parseFile (head args)

  g <- getStdGen
  let st = randomRSt (Map.size mp)
      (idx, ng) = runState st g
  
  -- This would be prettier if it were Monady...
  let start = fst $ Map.elemAt idx mp
      (idx', ng') = runState (randomIdxSt ((Map.!) mp start)) ng

      mid = (((Map.!) mp start) !! idx')
      idx'' = fst $ runState (randomIdxSt ((Map.!) mp mid)) ng'

      end = (((Map.!) mp mid) !! idx'')

  print start
  print mid
  print end
