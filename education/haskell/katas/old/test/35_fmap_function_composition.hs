{-
    instance Functor ((->) r) where
        fmap = (.)
-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "fmap can be considered to be function composition" $ do
        it "works with 2 arguments" $ do
            pending
            {- ((*__) . (+___) $ 1) `shouldBe` 303 -}
            {- (fmap (*__) (+___) 1) `shouldBe` 303 -}
            {- ((*__) `fmap` (+___) $ 1) `shouldBe` 303 -}
            {- (fmap (show . (*__)) (*___) 1) `shouldBe` "300" -}
