{-# LANGUAGE UnicodeSyntax #-}

module Ex46_FaMaybeMonoidsSpec
  ( spec
  ) where

import Data.Monoid
import Test.Hspec
import Data.Monoid.Unicode
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Map.Unicode
import Data.Sequence.Unicode
import Control.Applicative.Unicode

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Maybe Monoids" $ do
        it "works with `mappend` Maybe values" $ do
             Nothing ⊕ Just "andy" `shouldBe` Just "andy"
        it "can use the abbreviated mappend form '<>'" $ do
             Just LT <> Nothing `shouldBe` Just LT
             Just (Sum 3) <> Just (Sum 4)
                 `shouldBe` Just (Sum 7)
        it "can use First if the type is not instance of Monoids" $ do
             (getFirst $ First (Just 'a') <> First (Just 'b'))
                 `shouldBe` Just 'a'
             (getFirst $ First Nothing <> First (Just 'b'))
                 `shouldBe` Just 'b'
             (getFirst $ First (Just 'a') <> First Nothing)
                 `shouldBe` Just 'a'
        it "can use First to find the first Just value" $ do
             (getFirst. mconcat . map First $ [Nothing, Just 9, Just 10])
                 `shouldBe` Just 9
             ((getFirst . mconcat . map First  $ [Nothing, Nothing]) :: Maybe Int)
                 `shouldBe` Nothing
        it "can use Last to find the last Just value" $ do
             (getLast . mconcat . map Last $ [Nothing, Just 9, Just 10, Nothing])
                 `shouldBe` Just 10
        it "can find the first and last matching value from a list" $ do
             let interesting = [ 'a', 'b' ]
             let q c = if c `elem` interesting then Just c else Nothing
             q 'a' `shouldBe` Just 'a'
             q 'c' `shouldBe` Nothing
            --- Create First of 'q' product
             getFirst (mconcat $ map (First . q) "cabinet") `shouldBe` Just 'a'
             getLast (mconcat $ map (Last . q) "cabinet") `shouldBe` Just 'b'
