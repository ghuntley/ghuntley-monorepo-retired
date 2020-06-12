{-# LANGUAGE UnicodeSyntax #-}

module Ex08_FlowWhereSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec
    -- | bmi <= skinny = "You're underweight, you emo, you!"
    -- | bmi <= normal = "You're supposedly normal."
    -- | bmi <= fat = "You're fat! Lose some weight!"
    -- | otherwise   = "You're a whale, congratulations!"

-- BMI boundaries are 18.5, 25.0 and 30.0
-- Calculation logic: weight / height ^ 2
-- bmiTell weight height
-- initials :: String -> String -> String
-- calcBmis :: Fractional t => [(t, t)] -> [t]

bmiTell wt ht
     | bmi <= skinny = "You're underweight, you emo, you!"
     | bmi <= normal = "You're supposedly normal."
     | bmi <= fat = "You're fat! Lose some weight!"
     | otherwise   = "You're a whale, congratulations!"
     where bmi = wt / (ht ^ 2)
           skinny = 18.5
           normal = 25.0
           fat = 30.0

initials :: String -> String -> String
initials  fn ln
  | null fn && null ln = ""
  | null fn = [head ln]
  | null ln = [head fn]
  | otherwise = [ head fn, head ln]


calcBmis :: Fractional t => [(t, t)] -> [t]
calcBmis [] = []
calcBmis ((x,y):xs) = (x / (y^2)): calcBmis xs


spec :: Spec
spec =
    describe "where - to DRY up logic" $ do
        it "can calculate BMI from values" $ do
             bmiTell 85 1.90 `shouldBe` "You're supposedly normal."
        it "can extract initials from a string" $ do
             initials "" "" `shouldBe` ""
             initials "Attila" "Domokos" `shouldBe` "AD"
        it "can be used in list comprehensions" $ do
             calcBmis [(85, 1.90)] `shouldBe` [23.545706371191137]
