import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{- BMI boundaries are 18.5, 25.0 and 30.0 -}
{- Calculation logic: weight / height ^ 2 -}
{- bmiTell weight height -}
    {- | bmi <= skinny = "You're underweight, you emo, you!" -}
    {- | bmi <= normal = "You're supposedly normal." -}
    {- | bmi <= fat = "You're fat! Lose some weight!" -}
    {- | otherwise   = "You're a whale, congratulations!" -}

{- initials :: String -> String -> String -}

{- calcBmis :: Fractional t => [(t, t)] -> [t] -}

main :: IO()
main = hspec $ do
    describe "where - to DRY up logic" $ do
        it "can calculate BMI from values" $ do
            pending
            {- bmiTell 85 1.90 `shouldBe` "You're supposedly normal." -}
        it "can extract initials from a string" $ do
            pending
            {- initials "" "" `shouldBe` "" -}
            {- initials "Attila" "Domokos" `shouldBe` "AD" -}
        it "can be used in list comprehensions" $ do
            pending
            {- calcBmis [(85, 1.90)] `shouldBe` [23.545706371191137] -}
