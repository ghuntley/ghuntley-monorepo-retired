module Ex66_ExercisesBalancedBinaryTreeSpec
  ( spec
  ) where

import Test.Hspec

import Data.List

main :: IO ()
main = hspec spec

{-
Given this tree:
      20
  10      30
5   15  18  35
-}
data Tree a
  = Leaf
  | Node a
         (Tree a)
         (Tree a)
  deriving (Show, Eq)

tree :: Tree Int
tree =
  Node
    20
    (Node 10 (Node 5 Leaf Leaf) (Node 15 Leaf Leaf))
    (Node 30 (Node 22 Leaf Leaf) (Node 35 Leaf Leaf))

invalidTree :: Tree Int
invalidTree =
  Node
    20
    (Node 10 (Node 5 Leaf Leaf) (Node 15 Leaf Leaf))
    (Node 30 (Node 18 Leaf Leaf) (Node 35 Leaf Leaf))

allNodes :: (Int -> Bool) -> Tree Int -> Bool
allNodes _ Leaf = True
allNodes f (Node n left right) = f n && allNodes f left && allNodes f right

isBalanced :: Tree Int -> Bool
isBalanced tree = all (== True) (balanced tree)

balanced :: Tree Int -> [Bool]
balanced (Node x Leaf Leaf) = [True]
balanced (Node x left right) =
  let balancedValue = allNodes (< x) left && allNodes (> x) right
   in balancedValue : (balanced left ++ balanced right)

spec :: Spec
spec =
  describe "Balanced" $ do
    it "can tell if everything on the left is lower" $ do
      let (Node x left right) = tree
      allNodes (< x) left `shouldBe` True
    it "can tell if everything on the right is higher" $ do
      let (Node x left right) = tree
      allNodes (> x) right `shouldBe` True
    it "can tell if a tree is balanced" $ do
      isBalanced tree `shouldBe` True
      isBalanced invalidTree `shouldBe` False
