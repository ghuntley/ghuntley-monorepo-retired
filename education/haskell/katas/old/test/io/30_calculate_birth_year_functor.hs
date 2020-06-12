import Text.Read (readMaybe)

displayAge maybeAge =
    case maybeAge of
         Nothing -> putStrLn "You provided an invalid year"
         Just age -> putStrLn $ "In 2020, you will be: " ++ show age

yearToAge year = 2020 - year

main = do
    putStrLn "Please enter your birth year"
    yearString <- getLine
    let maybeAge = do
            yearInteger <- readMaybe yearString
            return $ yearToAge yearInteger
    displayAge maybeAge

{-
-- 4. - use fmap
import Text.Read (readMaybe)

displayAge maybeAge =
    case maybeAge of
         Nothing -> putStrLn "You provided an invalid year"
         Just age -> putStrLn $ "In 2020, you will be: " ++ show age

yearToAge year = 2020 - year

main = do
    putStrLn "Please enter your birth year"
    yearString <- getLine
    let maybeAge = fmap yearToAge (readMaybe yearString)
    displayAge maybeAge
-}

{-
--3
import Text.Read (readMaybe)

displayAge maybeAge =
    case maybeAge of
         Nothing -> putStrLn "You provided an invalid year"
         Just age -> putStrLn $ "In 2020, you will be: " ++ show age

yearToAge year = 2020 - year

main = do
    putStrLn "Please enter your birth year"
    yearString <- getLine
    let maybeAge =
            case readMaybe yearString of
                Nothing -> Nothing
                Just year -> Just (yearToAge year)
    displayAge maybeAge
-}

{-
-- 2.
import Text.Read (readMaybe)

main = do
    putStrLn "Please enter your birth year"
    yearString <- getLine
    case readMaybe yearString of
         Nothing -> putStrLn "You provided an invalid year"
         Just year -> putStrLn $ "In 2020, you will be: " ++ show (2020 - year)
-}

{-
-- 1.
main = do
    putStrLn "Please enter your birth year"
    year <- getLine
    putStrLn $ "In 2020, you will be: " ++ show (2020 - read year)
-}
