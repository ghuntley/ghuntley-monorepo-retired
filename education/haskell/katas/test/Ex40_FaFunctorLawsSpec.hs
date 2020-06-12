{-# LANGUAGE UnicodeSyntax #-}
module Ex40_FaFunctorLawsSpec
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
    ~~The first functor law:~~
    states that if we map the `id` function over a functor, the functor that
    we get back should be the same as the original functor.
        fmap id = id

    ~~The second functor law:~~
    says the componsing two functions and then mapping the result function
    over a functor should be the same as first mapping one function over
    the functor and then mapping the other one.
        fmap (f . g) = fmap f . fmap g
-}

spec :: Spec
spec = do
    describe "fmap laws" $ do
        it "works with the first law" $ do
             fmap id(Just 3) `shouldBe` Just 3
             id (Just 3) `shouldBe` Just 3
             fmap id [1..5] `shouldBe` [1,2,3,4,5]
             id [1..5]  `shouldBe` [1,2,3,4,5]
             fmap id ([] :: [Int]) `shouldBe` []
             fmap id (Nothing :: Maybe Bool) `shouldBe` Nothing
