{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE InstanceSigs #-}

module Ex59_MonadsInstancesSpec
  ( spec
  ) where

import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode
import Control.Monad.Unicode

main :: IO ()
main = hspec spec

-- Add the Functor, Applicative and Monad instances
-- for MyMaybe custom data type

data MyMaybe a = MyNothing | MyJust a
    deriving (Show, Eq)

instance Functor MyMaybe where
    fmap :: (a -> b) -> MyMaybe a -> MyMaybe b
    fmap f (MyJust x) = MyJust (f x)
    fmap _ MyNothing = MyNothing

instance Applicative MyMaybe where
    pure = MyJust
    MyNothing <*> _ = MyNothing
    (MyJust f) <*> x = fmap f x

instance Monad MyMaybe where
    return = MyJust
    (MyJust x) >>= f = f x
    MyNothing >>= f = MyNothing

spec :: Spec
spec = do
    describe "Monad Examples" $ do
        it "works for Functor" $ do
             fmap (+2) MyNothing `shouldBe` MyNothing
             fmap (+2) (MyJust 3) `shouldBe` MyJust 5
        it "works for Applicative" $ do
             pure 2 `shouldBe` MyJust 2
             (+) <$> MyJust 2 <*> MyJust 5
                 `shouldBe` MyJust 7
             MyJust (+2) <*> MyJust 5
                 `shouldBe` MyJust 7
        it "works for Monad" $ do
             return 2 `shouldBe` MyJust 2
             ((MyJust 10) >>= (\x -> (MyJust (x - 2))))
                 `shouldBe` MyJust 8
             (MyNothing >>= (\x -> (MyJust (x - 2))))
                 `shouldBe` MyNothing
             ((MyJust 10) >>= \_ -> (MyNothing :: MyMaybe Int))
                 `shouldBe` MyNothing
