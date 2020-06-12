{-
    Appends a line to the file we already have.
    appendFile :: FilePath -> String -> IO ()
-}

import System.IO

main = do
    todoItem <- getLine
    ___ "test/io/todo.txt" ___
