-- From: http://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads
-- 6. Using the short-hand <$>
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
    let maybeAge = yearDiff
            <$> readMaybe futureYearString
            <*> readMaybe birthYearString
    displayAge maybeAge

{-
-- 5. Introducing Applicative functors
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
            fmap yearDiff (readMaybe futureYearString)
            <*> readMaybe birthYearString
    displayAge maybeAge
-}

{-
-- 4. Use 2 fmaps
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
            yearToAge <- fmap yearDiff (readMaybe futureYearString)
            fmap yearToAge (readMaybe birthYearString)
    displayAge maybeAge
-}

{-
-- 3. Use fmap
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
            yearToAge <- fmap yearDiff (readMaybe futureYearString)
            birthYear <- readMaybe birthYearString
            return $ yearToAge birthYear
    displayAge maybeAge
-}

{-
-- 2. Nicer, but not perfect
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
            birthYear <- readMaybe birthYearString
            futureYear <- readMaybe futureYearString
            return $ yearDiff futureYear birthYear
    displayAge maybeAge
--}

{-
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
            case readMaybe birthYearString of
                Nothing -> Nothing
                Just birthYear ->
                    case readMaybe futureYearString of
                         Nothing -> Nothing
                         Just futureYear -> Just (futureYear - birthYear)
    displayAge maybeAge
-}
