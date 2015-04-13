module Main where

import Data.List

data T = Start | Stop | Word String
       deriving (Show, Eq, Ord)

type Tokens = [T]

-- ! Convert a string to a list of T elements.
stringToT :: String -> Tokens
stringToT str = Start : map Word (words str) ++ [Stop]

-- ! Create a list of all the pairings that occur in [T]
pairings :: Tokens -> [(T,T)]
pairings ts = zip ts (drop 1 ts)

-- ! Count up the occurrences of pairings
occurrences :: [(T, T)] -> [(Int, (T,T))]
occurrences ts = map condense $ group $ sort ts
  where
    condense (x:xs) = (length xs + 1, x)
    
