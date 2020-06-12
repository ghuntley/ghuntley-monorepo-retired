import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{- Day is an enumeration of each day in the week -}

main :: IO()
main = hspec $ do
    describe "Derived Instances" $ do
        it "can compare two Bool fields" $ do
            pending
            {- Comparing True to False should be greater -}
            {- ___ ___ False `shouldBe` GT -}
            {- ___ > False `shouldBe` True -}
            {- ___ < False `shouldBe` False -}
        it "can compare Maybe values" $ do
            pending
            {- ___ < Just 100 `shouldBe` True -}
            {- Nothing > ___ (-49999) `shouldBe` False -}
            {- ___ 3 `compare` ___ 2 `shouldBe` GT -}
        it "can be part of Enum typeclass as all value constructors are nullary" $ do
            pending
            {- ___ Wednesday `shouldBe` "Wednesday" -}
            {- ___ "Saturday" `shouldBe` Saturday -}
        it "can be compared as it's part of Eq and Ord type classes" $ do
            pending
            {- ___ == Sunday `shouldBe` False -}
            {- ___ == Monday `shouldBe` True -}
            {- ___ < Wednesday `shouldBe` True -}
            {- ___ ___ Tuesday `shouldBe` LT -}
        it "is also part of Bounded, can get lowest and highest value" $ do
            pending
            {- (___ :: Day) `shouldBe` Monday -}
            {- (___ :: Day) `shouldBe` Sunday -}
        it "is an instance of Enum, can get predecessor and successors" $ do
            pending
            {- Enums have predecessors and successors -}
            {- ___ Monday `shouldBe` Tuesday -}
            {- ___ Saturday `shouldBe` Friday -}
            {- Calling predecessor for Monday will throw an error -}
            {- ___ (___ Monday) `shouldThrow` anyErrorCall -}

