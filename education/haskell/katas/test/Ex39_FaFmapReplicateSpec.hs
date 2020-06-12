{-# LANGUAGE UnicodeSyntax #-}
module Ex39_FaFmapReplicateSpec
  ( spec
  ) where


import Test.Hspec
import Test.QuickCheck
import Data.Either
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Map.Unicode
import Data.Sequence.Unicode

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "fmap replicate on various types" $ do
        it "can be used over list of ints" $ do
             fmap (replicate 3) [1,2,3]
                 `shouldBe` [[1,1,1],[2,2,2],[3,3,3]]
        it "can be used on Maybe values" $ do
             fmap (replicate 3) (Just 4)
                 `shouldBe` Just [4,4,4]
        it "can fmap on Nothing" $ do
             (fmap (replicate 3) Nothing :: Maybe [Int])
                 `shouldBe` Nothing
        it "can be used on Either" $ do
             (fmap (replicate 3) (Right "blah") :: Either String [String])
                 `shouldBe` Right ["blah","blah","blah"]
             (fmap (replicate 3) (Left "blah") :: Either String String)
                 `shouldBe` Left "blah"
