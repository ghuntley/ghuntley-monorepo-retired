{-
    Get arguments from the command line
    * getArgs will ge the arguments
    * getProgName will get the - you know it
 -}

import System.Environment
import Data.List

main = do
    args <- ___
    progName <- ___
    putStrLn "The arguments are:"
    mapM ___ ___
    putStr "The program name is: "
    putStrLn ___
