{-
    Read a non-existing file
    The `toTry` function should print the number of lines in the file:
        "The file has 3 lines!"
-}
import System.Environment
import System.IO
import Control.Exception

main = toTry `catch` handler

{- toTry :: IO () -}
___

{- handler :: IOError -> IO () -}
___
