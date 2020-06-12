{-
    Deletes an item from the todo list
    * Use the temp/io/todo.txt file
    * The tempfile should use "." and "temp"

 -}

import System.IO
import System.Directory
import Data.List

main = do
    handle <- ___
    (tempName, tempHandle) <- ___
    contents <- ___ handle
    let todoTasks = ___
        numberedTasks = ___
    putStrLn "There are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = ___
    hPutStr tempHandle $ ___
    hClose ___
    hClose ___
    removeFile ___
    renameFile ___ ___
