import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{-
    Typeclasses are like interfaces. A type class defines some
    behavior, and types can behave that way.
    When we say a type is an instance of a type class, we mean
    that we can use the functions that the typeclass defines with
    that type.
-}

{- data TrafficLight = ___ -}

{- Instead of deriving class instances for it, writing up instances by hand -}
{- instance Eq TrafficLight where -}
    {- ___ -}

{-
    Minimal complete definition for the typeclass = the minimum of functions that we
    have to implement so that our type can behave like the class advertises.
-}

{- Similarly, the instance of Show can be created by hand -}
{- instance Show TrafficLight where -}
    {- ___ -}

main :: IO()
main = hspec $ do
    describe "Recursive Data Structures" $ do
        it "can use the manually created Eq instance" $ do
            pending
            {- Red == Red `shouldBe` True -}
            {- Yellow == Green `shouldBe` False -}
            {- Yellow /= Red `shouldBe` True -}
        it "can use the manually created Show instance" $ do
            pending
            {- show Red `shouldBe` "Red light" -}
            {- show Green `shouldBe` "Green light" -}
