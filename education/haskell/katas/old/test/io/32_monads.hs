{-
    Why would anyone use monads, when there functors and applicative functors?
    The terse answer - context sensitivity: with a monad, you can make decisions
    on which processing path to follow based on previous results. With applicative
    functors, you have to always apply the same functions.

    Let's look at this example: if the future year is less than the birth year, we'll
    assume that the use just confused and entered the values in reverse, this code will
    automatically fix it by reversing the arguments to `yearDiff`. With a do notation and
    an if statement, it's easy.
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
    let maybeAge = ...
            -- auto-correct the user input:
            --   if the birthYear is higher than futureYear, swap the two
    displayAge maybeAge
