{-# LANGUAGE UnicodeSyntax #-}

module Ex62_MonadsLoggingWithWriterSpec
  ( spec
  ) where

import Control.Monad.Writer
import Data.Monoid
import Test.Hspec
import Control.Monad.Writer
import Data.Monoid
import Prelude.Unicode
import Data.Monoid.Unicode
import Control.Monad.Unicode

main :: IO ()
main = hspec spec

-- Greatest common deviser
gcd' :: Int -> Int -> Int
gcd' a b
  | b ≡ 0 = a
  | otherwise = gcd' b (a `mod` b)

gcdWithLog :: Int -> Int -> Writer [String] Int
gcdWithLog a b
  | b ≡ 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcdWithLog b (a `mod` b)

newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (f . g)

instance Monoid (DiffList a) where
  mempty = DiffList (\xs → [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (f ∘ g)

gcdWithDiffList :: Int -> Int -> Writer (DiffList String) Int
gcdWithDiffList a b
  | b ≡ 0 = do
      tell (toDiffList ["Finished with " ++ show a])
      return a
  | otherwise = do
      result ← gcdWithDiffList b (a `mod` b)
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result

spec :: Spec
spec = do
    describe "Logging with Writer" $ do
        it "can Find the greatest common devisor" $ do
             gcd' 8 3 `shouldBe` 1
        it "can decorate the gcd function with logging" $ do
             (fst $ runWriter $ gcdWithLog 8 3) `shouldBe` 1
             (snd $ runWriter $ gcdWithLog 8 3)
                 `shouldBe` ["8 mod 3 = 2","3 mod 2 = 1","2 mod 1 = 0","Finished with 1"]
        it "can efficiently append to difference list" $ do
             (fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3]))
                 `shouldBe` [1,2,3,4,1,2,3]
        it "can log with DiffList String" $ do
             (fromDiffList . snd . runWriter $ gcdWithDiffList 8 3)
                `shouldBe` ["Finished with 1","2 mod 1 = 0","3 mod 2 = 1","8 mod 3 = 2"]

{-
    To run it in the RePL:
    ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdWithDiffList 110 34
-}
-- -*- flycheck-idle-change-delay: 10; -*-
