import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{-
    This would work:
    data ZipList a = ZipList { getZipList :: [a] }

    But this is better:
    newtype ZipList a = ZipList { getZipList :: [a] }
    * faster (no boxing/unboxing)
    * it can have only one value constructor

    Making a tuple to be an instance of a Functor is not possible.
    We can newtype the tuple in a way that the second type
    parameter represents the type of the first component of the tuple.
-}

{- newtype CharList = ___ -}

{- newtype Pair ... = ___ -}

{- instance Functor (Pair c) where -}
    {- fmap ___ -}

main :: IO ()
main = hspec $ do
    describe "newtype" $ do
        it "can print values" $ do
            pending
            {- let charList = ___ -}
            {- show charList -}
                {- `shouldBe` "CharList {getCharList = \"this will be shown!\"}" -}
        it "can equate values" $ do
            pending
            {- CharList "benny" == CharList "benny" -}
                {- `shouldBe` ___ -}
            {- CharList "benny" == CharList "oisters" -}
                {- `shouldBe` ___ -}
        it "works with the newtype Pair" $ do
            pending
            {- (getPair $ fmap (*100) (Pair (2,3))) -}
                {- `shouldBe` (200, 3) -}
            {- (getPair $ fmap reverse (Pair ("london calling", 3))) -}
                {- `shouldBe` ("gnillac nodnol", 3) -}
