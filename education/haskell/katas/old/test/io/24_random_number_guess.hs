{-
    Computer guesses a number, user try to find it out
    randomR (1,10) gen :: (Int, StdGen) will generate the number between 1 - 10
        plus it returns the new Gen
-}

import System.Random
import Control.Monad(when)

main = do
    gen <- getStdGen
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = ___
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (___ $ ___ ___) $ do
        let number = ___ ___
        if randNumber == number
           then putStrLn "You are correct!"
           else putStrLn $ "Sorry, it was " ++ show randNumber
        ___ ___  {- Recur -}

{- Another way to do it just in the main function -}
{-
main = do
    gen <- getStdGen
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)
    putStr "Which number in the range from 1 to 10 am I thinking of? "
    numberString <- getLine
    when (not $ null numberString) $ do
        let number = read numberString
        if randNumber == number
           then putStrLn "You are correct!"
           else putStrLn $ "Sorry, it was " ++ show randNumber
        newStdGen
        main
-}
