import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{- BMI boundaries are 18.5, 25, 30 -}
{- bmiTell :: (Ord a, Fractional a) => a -> String -}

{- max' :: (Ord a) => a -> a -> a -}

{- compare' :: (Ord a) => a -> a -> Ordering -}

main :: IO()
main = hspec $ do
    describe "guards, guards!" $ do
        it "can be considered as a conditional, but easier to read" $ do
            pending
            {- bmiTell 15.2 `shouldBe` "You're underweight, you emo, you!" -}
            {- bmiTell 23 `shouldBe` "You're supposedly normal." -}
            {- bmiTell 27 `shouldBe` "You're fat! Lose some weight!" -}
            {- bmiTell 31.4 `shouldBe` "You're a whale, congratulations!" -}
        it "can calculate max from two values" $ do
            pending
            {- max' 1 3 `shouldBe` 3 -}
        it "can compare two values" $ do
            pending
            {- compare' 2 2 `shouldBe` EQ -}
            {- 2 `compare'` 3 `shouldBe` LT -}
