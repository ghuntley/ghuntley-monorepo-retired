{-
    Read a non-existing file
    The `toTry` should function as it did in the previous example
    But the handler should use pattern matching
        handle the `isDoesNotExistError` case
-}

import System.Environment
import System.IO
import System.IO.Error
import Control.Exception (catch)

main = toTry `catch` handler

{- toTry :: IO () -}
_____

{- handler :: IOError -> IO () -}
_____
