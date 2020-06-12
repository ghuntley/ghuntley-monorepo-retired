import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{-
    Simulate the JavaScript behavior,
    where if (0) or if ("") if ("WHAT") works.
-}

{- Define a YesNo type class
   that returns boolean value based on type -}

{- Define some instances -}
{- For Int -}

{- For Lists -}

{- For Bool -}

{- For Maybe -}

data TrafficLight = Red | Yellow | Green deriving (Eq)

{- Create derivied instance of YesNo for TrafficLight -}

{- yesnoIf :: (YesNo y) => y -> a -> a -> a -}

main :: IO()
main = hspec $ do
    describe "Yes/No typeclass" $ do
        it "works with Bool fields" $ do
            pending
            {- yesno False `shouldBe` False -}
        it "works with Ints" $ do
            pending
            {- yesno (0 :: Int) `shouldBe` False -}
            {- yesno (1 :: Int) `shouldBe` True -}
        it "works with Lists" $ do
            pending
            {- yesno [] `shouldBe` False -}
            {- yesno [3,4] `shouldBe` True -}
        it "works the type TrafficLight" $ do
            pending
            {- yesno Red `shouldBe` False -}
            {- yesno Green `shouldBe` True -}
        it "can do a conditional with yesno" $ do
            pending
            {- yesnoIf Red "true" "false" `shouldBe` "false" -}
            {- yesnoIf [] 1 2 `shouldBe` 2 -}
            {- yesnoIf (Just 500) "YEAH!" "NO" `shouldBe` "YEAH!" -}
