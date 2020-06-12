{-# LANGUAGE UnicodeSyntax #-}

module Ex41_FaApplicativesSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Map.Unicode
import Data.Sequence.Unicode

main :: IO ()
main = hspec spec

{-
    functor definition
    class (Functor f) => Applicative f where
        pure :: a -> f a
        (<*>) :: f (a -> b) -> f a -> f b

    Applicative instance implementation for Maybe
    instance Applicative Maybe where
        pure = Just
        Nothing <*> _ = Nothing
        (Just f) <*> something = fmap f something

    (<$>) :: (Functor f) => (a -> b) -> f a -> f b
    f <$> x = fmap f x
-}

spec :: Spec
spec = do
    describe "Applicative" $ do
        it "applies function inside the Just" $ do
            -- Hint: use lambda expression
             (fmap (*2) $ Just 3)
                 `shouldBe` (Just 6)
             (*) <$> Just 2 <*> Just 3 `shouldBe` Just 6
        it "applies function in list" $ do
             let a = fmap (*) [1..4]
             fmap (\f -> f 9) a `shouldBe` [9,18,27,36]
        it "works with Maybe" $ do
             Just (+3) <*> Just 9
                 `shouldBe` Just 12
             pure (+8) <*> Just 5
                 `shouldBe` Just 13
             Just (++"hahah") <*> Nothing
                 `shouldBe` Nothing
             Nothing <*> Just "woot"
                 `shouldBe` (Nothing :: Maybe String)
        it "operates on several functors with a single function" $ do
             pure (+) <*> Just 3 <*> Just 5
                 `shouldBe` Just 8
             pure (+) <*> Just 3 <*> Nothing
                 `shouldBe` (Nothing :: Maybe Int)
        it "can use <$> as fmap with an infix operator" $ do
             (++) <$> Just "johntra"  <*> Just "volta"
                 `shouldBe` Just "johntravolta"
             (++) "johntra" "volta" `shouldBe` "johntravolta"
        it "works with a list of functions" $ do
             [(*0),(+100),(^2)] <*> [1,2,3]
                 `shouldBe` [0,0,0,101,102,103,1,4,9]
             [(+),(*)] <*> [1,2] <*> [3,4]
                 `shouldBe` [4,5,5,6,3,4,6,8]
        it "can be used as a replacement for list comprehensions" $ do
            {- example...
            [x*y | x <- [2,5,10], y <- [8,10,11]]
                `shouldBe` [16,20,22,40,50,55,80,100,110] -}
             (*) <$> [2,5,10] <*> [8,10,11]
                 `shouldBe` [16,20,22,40,50,55,80,100,110]
            -- Keep only the values that are greater than 50 of the product
             (filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11])
                 `shouldBe` [55,80,100,110]

