{-# LANGUAGE UnicodeSyntax #-}
module Ex31_TypesMoreTypeClassesSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Map.Unicode

main :: IO ()
main = hspec spec

{-
    Typeclasses are like interfaces. A type class defines some
    behavior, and types can behave that way.
    When we say a type is an instance of a type class, we mean
    that we can use the functions that the typeclass defines with
    that type.
-}

data TrafficLight = Red | Yellow | Green

{- Instead of deriving class instances for it, writing up instances by hand -}
{- instance Eq TrafficLight where -}
instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False  

{-
    Minimal complete definition for the typeclass = the minimum of functions that we
    have to implement so that our type can behave like the class advertises.
-}

{- Similarly, the instance of Show can be created by hand -}
{- instance Show TrafficLight where -}
    {- ___ -}
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"
    
spec :: Spec
spec = do
    describe "Recursive Data Structures" $ do
        it "can use the manually created Eq instance" $ do
             Red == Red `shouldBe` True
             Yellow == Green `shouldBe` False
             Yellow /= Red `shouldBe` True
        it "can use the manually created Show instance" $ do
             show Red `shouldBe` "Red light"
             show Green `shouldBe` "Green light"
