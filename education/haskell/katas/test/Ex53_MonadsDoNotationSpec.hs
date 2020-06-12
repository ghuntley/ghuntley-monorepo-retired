{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ex53_MonadsDoNotationSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Prelude.Unicode
import Data.Monoid.Unicode
import Control.Monad.Unicode

main :: IO ()
main = hspec spec

foo :: Maybe String
foo = Just 3 >>= (\x ->
                    Just "!" >>= (\y ->
                                    Just (show x ++ "!")))

-- USE do expression
fooMonad :: Maybe String
fooMonad = do
  x ← Just 3
  
  y ← Just "!"
  return $ show x ++ y

-- Use do expression here as well
marySue :: Maybe Bool
marySue = do
  x ← Just 9
  Just (x>8)

spec :: Spec
spec = do
    describe "The do notation" $ do
        it "can combine functions with Maybe values" $ do
            -- Use a lambda here
             (Just 3 >>= (\x → Just (show x ++ "!")))
                 `shouldBe` Just "3!"
             (Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y))))
                 `shouldBe` Just "3!"
             (Just 3 >>= (\x -> Just "!" >>= (\y -> Nothing)))
                 `shouldBe` (Nothing :: Maybe String)
        it "can be wrapped on different lines" $ do
             foo `shouldBe` Just "3!"
             fooMonad `shouldBe` Just "3!"
        it "can simplify more complex expressions" $ do
            -- Use a lambda here for the prdicate
             (Just 9 >>= (\x → Just $ x>8)) `shouldBe` Just True
             marySue `shouldBe` Just True
