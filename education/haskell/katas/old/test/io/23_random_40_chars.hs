{-
    Generate random 40 chars as 2 sets of 20s
    getStdGen will get a random generator
    newStdGen will get a new one if you need it
-}
import System.Random
import Data.List

main = do
    gen <- ___
    putStrLn $ ___
    gen' <- ___
    putStrLn $ ___

{- Another way to solve it -}
{- main = do -}
    {- gen <- getStdGen -}
    {- let randomChars = randomRs ('a', 'z') gen -}
        {- (first20, rest) = splitAt 20 randomChars -}
        {- (second20, _) = splitAt 20 rest -}
    {- putStrLn first20 -}
    {- putStrLn second20 -}
