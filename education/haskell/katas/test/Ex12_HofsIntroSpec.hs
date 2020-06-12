module Ex12_HofsIntroSpec
  ( spec
  ) where

import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode

main :: IO ()
main = hspec spec


compareWithHundred x = compare 100 x

divideByTen x = (fromIntegral x) / 10.0

isUpperAlphanum x = x ∈ ['A'..'Z']

applyTwice f x = f $ f x


zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y: zipWith' f xs ys


map' _ [] = []
map' f (x:xs) = f x : map' f xs


filter' _ [] = []
filter' p (x:xs) = if (p x ≡ True) then x : filter' p xs else filter' p xs

-- chain ∷ Integral a ⇒ a → [a]
chain n
  | n ≡ 1 = [1]
  | even n = n : chain (div n 2)
  | odd n = n : chain (3 ⋅ n + 1)

spec :: Spec
spec = do
    describe "Higher Order Functions" $ do
        it "can copare it with 100" $ do
             compareWithHundred 100 `shouldBe` EQ
        it "can divide by 10 with partial function" $ do
             divideByTen 200 `shouldBe` 20.0
        it "can tell if upper-case letter" $ do
             isUpperAlphanum 'b' `shouldBe` False
             isUpperAlphanum 'B' `shouldBe` True
        it "can apply a function twice" $ do
             applyTwice (+3) 10 `shouldBe` 16
             applyTwice (++ " HAHA") "HEY" `shouldBe` "HEY HAHA HAHA"
        it "can zip with a function" $ do
             zipWith' (+) [1,2,3] [4,5,6] `shouldBe` [5,7,9]
             zipWith' (*) [1,2,3] [4,5,6] `shouldBe` [4,10,18]
             zipWith' max [1,2,3] [2,1,3] `shouldBe` [2,2,3]
        it "can map over a list of values" $ do
             map' (+3) [] `shouldBe` []
             map' (+3) [1,2,3,4] `shouldBe` [4,5,6,7]
             map' ((*2).(+3)) [1,2,3,4] `shouldBe` [8,10,12,14]
             [x+3 | x <- [1,2,3,4]] `shouldBe` [4,5,6,7]
             map' fst [(1,2),(3,4),(5,6)] `shouldBe` [1,3,5]
        it "can filter items from a list" $ do
             filter' (>3) [] `shouldBe` []
             filter' (>3) [1..5] `shouldBe` [4,5]
             filter' (==3) [1..5] `shouldBe` [3]
        it "can calculate the Collatz sequenses" $ do
            {- if it's even, divide by two -}
            {- it it's odd, multiply by 3 and add 1 to it -}
             chain 1 `shouldBe` [1]
             chain 10 `shouldBe` [10,5,16,8,4,2,1]
