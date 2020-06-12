{-
    Use System.IO to generate random numbers
-}

import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen)
