import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

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

main :: IO ()
main = hspec $ do
    describe "fmap laws" $ do
        it "works with the first law" $ do
            pending
            {- fmap _ (Just 3) `shouldBe` Just 3 -}
            {- id (___) `shouldBe` Just 3 -}
            {- fmap __ ___ `shouldBe` [1,2,3,4,5] -}
            {- id ___  `shouldBe` [1,2,3,4,5] -}
            {- fmap __ ([] :: [Int]) `shouldBe` [] -}
            {- fmap __ (Nothing :: Maybe Bool) `shouldBe` ___ -}
