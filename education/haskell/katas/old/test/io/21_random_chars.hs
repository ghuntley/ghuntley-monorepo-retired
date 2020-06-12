{-
    Generate 10 random characters,
    which are the same random number every time we call it,
    as it's using a static generator
    randomRs ('a','z') (mkStdGen Int) :: String - will generate random chars
 -}
import System.Random

generateRandomChars :: String
___

main = do
    putStrLn $ "The generated random string: " ++ generateRandomChars
