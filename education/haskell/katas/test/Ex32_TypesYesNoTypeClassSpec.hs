{-# LANGUAGE UnicodeSyntax #-}
module Ex32_TypesYesNoTypeClassSpec
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
    Simulate the JavaScript behavior,
    where if (0) or if ("") if ("WHAT") works.
-}

{- Define a YesNo type class
   that returns boolean value based on type -}
class YesNo a where
  yesno∷ a → Bool
  
{- Define some instances -}
{- For Int -}
instance YesNo Int where
  yesno 0 = False
  yesno _ = True
  
{- For Lists -}
instance YesNo [a] where
  yesno [] = False
  yesno _ = True

{- For Bool -}
instance YesNo Bool where
  yesno = id
{- For Maybe -}
instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

data TrafficLight = Red | Yellow | Green deriving (Eq)

{- Create derivied instance of YesNo for TrafficLight -}
instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True
  
yesnoIf :: (YesNo y) => y -> a -> a -> a 
yesnoIf yesNoVal yesResult noResult = if yesno yesNoVal then yesResult else noResult

spec :: Spec
spec = 
    describe "Yes/No typeclass" $ do
        it "works with Bool fields" $ do
             yesno False `shouldBe` False
        it "works with Ints" $ do
             yesno (0 :: Int) `shouldBe` False
             yesno (1 :: Int) `shouldBe` True
        it "works with Lists" $ do
             yesno [] `shouldBe` False
             yesno [3,4] `shouldBe` True
        it "works the type TrafficLight" $ do
             yesno Red `shouldBe` False
             yesno Green `shouldBe` True
        it "can do a conditional with yesno" $ do
             yesnoIf Red "true" "false" `shouldBe` "false"
             yesnoIf [] 1 2 `shouldBe` 2
             yesnoIf (Just 500) "YEAH!" "NO" `shouldBe` "YEAH!"
