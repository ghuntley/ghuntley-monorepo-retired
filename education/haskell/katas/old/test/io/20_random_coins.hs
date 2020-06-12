{-
    Simulate 3 random coin toss
-}
import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
___

main = do
    let (a,b,c) = threeCoins (mkStdGen 943)
        response = "First coin: " ++ show a ++
                   " second coin: " ++ show b ++
                   " third coin: " ++ show c
    putStrLn response
