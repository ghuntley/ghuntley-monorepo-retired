module Ex34_FaFunctorTypeClassSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec


{-
    Functor typeclass is for things that can be mapped over.
    This is how it's implemented:
    class Functor f where
        fmap :: (a -> b) -> f a -> f b
    f here is a type constructor
    Remember `Maybe Int` is a concrete type, `Maybe a` is type constructor.
    It takes a function from one type to the other, and a functor applied
    with one type, and functor applied with another type.

    Good old map is a functor: (a -> b) -> [a] -> [b]
    instance Functor [] where
        fmap = map
    [] is a type constructor

    Types that can act like a box can be functors

    Maybe is a functor as well:
    instance Functor Maybe where
        fmap f (Just x) = Just (f x)
        fmap f Nothing = Nothing

    Functor wants a type constructor that takes one type and not
    a concrete type.
    Only type constructors with one params can be used in functors.

    Types that can act like a box can be functors.

    Either is a functor
    instance Functor (Either a) where
        fmap f (Right x) = Right (f x)
        fmap f (Left x) = Left x
-}

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

{- Create an Functor implementation of the Tree -}

instance Functor Tree  where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

spec :: Spec
spec = do
    describe "Functor typeclass" $ do
        it "map is a functor" $ do
             map (*2) [1..3] `shouldBe` [2,4,6]
             fmap (*2) [1..3] `shouldBe` [2,4,6]
             fmap (*3) [] `shouldBe` []
        it "works with Maybe, as it's a functor" $ do
             fmap (++ " HEY GUYS") (Just "Something serious.")
                 `shouldBe` Just "Something serious. HEY GUYS"
             fmap (++ " HEY GUYS") Nothing `shouldBe` Nothing
             (fmap (*2) $ Just 200) `shouldBe` Just 400
             (fmap (*3) Nothing) `shouldBe` Nothing
        it "works with our Tree type class" $ do
             let nums = [20,28,12]
             let numsTree = foldr treeInsert EmptyTree nums

             fmap (*2) EmptyTree `shouldBe` EmptyTree
             fmap (*4) (foldr treeInsert EmptyTree [5,7,3])
                 `shouldBe` numsTree
