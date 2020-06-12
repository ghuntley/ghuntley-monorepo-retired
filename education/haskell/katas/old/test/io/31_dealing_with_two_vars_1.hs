{-
    Retrieve the two inputs
    In case the user input is invalid, just use Nothing,
    Otherwise use value
-}

-- 1.
import Text.Read (readMaybe)

displayAge maybeAge =
    case maybeAge of
         Nothing -> putStrLn "You provided an invalid year"
         Just age -> putStrLn $ "In that year, you will be: " ++ show age

main = do
    putStrLn "Please enter your birth year:"
    birthYearString <- getLine
    putStrLn "Please enter some year in the future"
    futureYearString <- getLine
    let maybeAge =
            ___
    displayAge maybeAge
