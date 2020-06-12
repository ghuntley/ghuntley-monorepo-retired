import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{- head' :: [a] -> a -}
{- head' xs = case xs of ... -}

{- describeList :: [a] -> String -}
{- describeList xs = "The list is " ++ case xs ___ -}
                                            {- ___ -}
                                            {- ___ -}

{- Case statement can be written with patten matching -}
{- describeList xs = "The list is " ++ what xs -}
    {- where ___ [] = "is empty." -}
          {- ___ -}
          {- ___ -}


main :: IO()
main = hspec $ do
    describe "Case expressions" $ do
        it "can be used anywhere" $ do
            pending
            {- head' [1,3] `shouldBe` 1 -}
        it "can be even used in expressions" $ do
            pending
            {- describeList [] `shouldBe` "The list is empty." -}
            {- describeList [1] `shouldBe` "The list is a singleton list." -}
            {- describeList [1,2] `shouldBe` "The list is a longer list." -}
