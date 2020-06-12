{-# LANGUAGE UnicodeSyntax #-}


module Ex18_ModulesIntroSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Data.List
import qualified Data.Set as M {- use M.filter now -}
import Prelude.Unicode
import Data.Monoid.Unicode

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Dealing with Modules" $ do
        it "can pick up and use the nub function from Data.List" $ do

            {- find the number of unique (`nub`) elements in a list -}
            (length $ nub [1,1,2,3,4,4]) `shouldBe` 4

        it "inserts an item in between the list" $ do

            {- hint: use the `intersperse` function -}
            (intersperse '.' "MONKEY") `shouldBe` "M.O.N.K.E.Y"
            (intersperse 0 [1,2,3,4]) `shouldBe` [1,0,2,0,3,0,4]
        it "inserts a list into a list and flattens it" $ do

            {- hint: use the `intercalate` function -}
             (intercalate " " ["hey","there","guys"])
                 `shouldBe` "hey there guys"
             (intercalate [0,0] [[1,2,3], [4,5,6]])
                 `shouldBe` [1,2,3,0,0,4,5,6]
        it "transposes a matrix" $ do
             (transpose [[1,2,3],[4,5,6],[7,8,9]])
                 `shouldBe` [[1,4,7],[2,5,8],[3,6,9]]
             (transpose ["hey","there","guys"])
                 `shouldBe` ["htg","ehu","yey","rs","e"]
        it "concatenates lists" $ do
             (concat ["foo","bar","car"]) `shouldBe` "foobarcar"
             (concat [[3,4,5],[6,7,8,9]])
                 `shouldBe` [3,4,5,6,7,8,9]
        it "concatenates and maps a list" $ do
             (concatMap(replicate 3) [1..3])
                 `shouldBe` [1,1,1,2,2,2,3,3,3]
        it "'and' can return True if all the elements are True" $ do
             (and $ map  (>4) [5,6,7,8]) `shouldBe` True
             (and $ map (==4) [4,4,3,4,4]) `shouldBe` False
        it "'or' can return True if any elements are True" $ do
            (or $ map (==4) [4,4,3,4,4]) `shouldBe` True
            (or $ map (>4) [1..3]) `shouldBe` False
        it "'all' and 'any' works like it's expected" $ do
             (any (==4) [2,3,5,6,1,4]) `shouldBe` True
             (all (>4) [2,3,5,6,1,4]) `shouldBe` False
             (all (`elem` ['A'..'Z']) "HEYGUYSwhatsup")
                 `shouldBe` False
