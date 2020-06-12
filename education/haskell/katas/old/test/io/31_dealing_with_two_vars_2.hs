{-
    Use do notation to read in values
-}

import Text.Read (readMaybe)

displayAge :: Show a => Maybe a -> IO ()
displayAge maybeAge =
    case maybeAge of
         Nothing -> putStrLn "You provided an invalid year"
         Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff :: Int -> Int -> Int
yearDiff futureYear birthYear = futureYear - birthYear

main = do
    putStrLn "Please enter your birth year:"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge = do
            ....
    displayAge maybeAge
