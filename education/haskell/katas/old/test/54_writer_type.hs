import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad.Writer
import Data.Monoid

{-
    newType Writer w a = Writer { runWriter :: (a, w) }
    It's wrapped in a newtype so that it can be made an instance
    of Monad and that its type is separate from a normal tuple.

    instance (Monoid w) => Monad (Writer w) where
        return x = Writer (x, mempty)
        (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v'))
-}

-- Print "Got number: " and the var
{- logNumber :: Int -> Writer [String] Int -}

-- Multiply 3 and 5, but log the numbers using do notation
{- multWithLog :: Writer [String] Int -}

-- Same as above, but `tell` that "Gonna multiply these two"
{- multWithLog' :: Writer [String] Int -}

main :: IO()
main = hspec $ do
    describe "Writer Type" $ do
        it "can pair up runWriter with different monoids" $ do
            pending
            {- (runWriter (return 3 :: Writer String Int)) -}
                {- `shouldBe` (3, ___) -}
            {- (runWriter (return 3 :: Writer (Sum Int) Int)) -}
                {- `shouldBe` (3, ___) -}
            {- (runWriter (return 3 :: Writer (Product Int) Int)) -}
                {- `shouldBe` (3, ___) -}
        it "can use do notation as well" $ do
            pending
            {- runWriter multWithLog -}
                {- `shouldBe` (15, ["Got number: 3", "Got number: 5"]) -}
        it "can 'tell' what's going on" $ do
            pending
            {- runWriter multWithLog' -}
                {- `shouldBe` (15, ["Got number: 3", "Got number: 5","Gonna multiply these two"]) -}
