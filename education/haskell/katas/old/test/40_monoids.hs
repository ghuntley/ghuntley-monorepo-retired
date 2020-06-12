import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Monoid

{-
    Look at this typeclass:
    class Monoid m where
        mempty :: m
        mappend :: m -> m -> m
        mconcat :: [m] -> m
        mconcat = foldr mappend mempty

        And for lists:
        instance Monoid [a] where
            mempty = []
            mappend = (++)

    "*" and ++ are true for monoid laws as they are associative:
    * The function takes two parameters
    * The parameters and the returned value have the same type
    * There exists such a value that does not change other values
      when used with the binary function.

    For product and sum Data.Monoid exposes Product and Sum
-}

main :: IO ()
main = hspec $ do
    describe "Monoids" $ do
        it "defines mappend and mempty" $ do
            pending
            {- [1,2,3] ___ [4,5,6] -}
                {- `shouldBe` [1,2,3,4,5,6] -}
            {- ("one" ___ "two") ___ "tree" -}
                {- `shouldBe` "onetwotree" -}
            {- "one" ___ ("two" ___ "tree") -}
                {- `shouldBe` "onetwotree" -}
            {- "one" ___ "two" `mappend` ___ -}
                {- `shouldBe` "onetwotree" -}
            {- "pang" `mappend` mempty `shouldBe` "pang" -}
            {- ___ [[1,2],[3,6],[9]] -}
                {- `shouldBe` [1,2,3,6,9] -}
            {- (___ :: [Int]) `shouldBe` [] -}
        it "defines Product" $ do
            pending
            {- (___ $ ___ 3 `mappend` Product 9) -}
                {- `shouldBe` 27 -}
            {- (getProduct $ Product 3 `mappend` ___) -}
                {- `shouldBe` 3 -}
            {- (___ . ___ . ___ Product $ [3,4,2]) -}
                {- `shouldBe` 24 -}
        it "and Sum newtypes" $ do
            pending
            {- (___ $ ___ 2 `mappend` ___ 9) `shouldBe` 11 -}
            {- (___ $ ___ `mappend` Sum 3) `shouldBe` 3 -}
            {- (___ . ___ . ___ Sum $ [1,2,3]) `shouldBe` 6 -}
        it "has Any and All" $ do
            pending
            {- (___ $ Any ___ `mappend` ___ False) `shouldBe` True -}
            {- (getAll $ mempty `mappend` ___ ___) `shouldBe` True -}
