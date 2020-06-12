{-# LANGUAGE UnicodeSyntax #-}
module Ex33_TypesSequenceSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import Data.Sequence((><), (<|), (|>))
import qualified Data.Foldable as Foldable
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Map.Unicode
import Data.Sequence.Unicode

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Sequence" $ do
        it "can initialize singleton Seq" $
            Seq.singleton 1 `shouldBe` Seq.fromList [1]
        it "can append and prepend" $ do
             1 ⊲ Seq.singleton 2
                 `shouldBe` Seq.fromList [1,2]
             Seq.singleton 1 ⊳ 2
                 `shouldBe` Seq.fromList [1,2]
        it "can combine two" $ do
             let left = Seq.fromList [1,3,3]
             let right = Seq.fromList [7,1]
             left ⋈ right
                 `shouldBe` Seq.fromList [1,3,3,7,1]
        it "can convert a sequence to a List" $ do
             let list = Foldable.toList (Seq.fromList [1,2,3])
             list `shouldBe` [1,2,3]
             let result = Foldable.foldl' (+) 0 (Seq.fromList [1,2,3])
             result `shouldBe` 6
