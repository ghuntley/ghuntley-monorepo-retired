import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{- Example 1 -}
{- Define a list that uses Cons to add elements to the list -}
{- Use infixr 5 -}
{- data List a = ___ -}

{- Define a Tree structure with EmptyTree and Node  -}
{- data Tree a = ___ -}

{- treeInsert :: (Ord a) => a -> Tree a -> Tree a -}

{- treeElem :: (Ord a) => a -> Tree a -> Bool -}

main :: IO()
main = hspec $ do
    describe "Recursive Data Structures" $ do
        {- Example 1 -}
        {- it "can define a List" $ do -}
            {- (5 `Cons` Empty) `shouldBe` Cons 5 Empty -}
            {- 4 `Cons` (5 `Cons` Empty) -}
                {- `shouldBe` Cons 4 (Cons 5 Empty) -}
        it "can set fixity declarations with infixr 5" $ do
            pending
            {- 5 :-: Empty `shouldBe` ((:-:) 5 Empty) -}
            {- 4 :-: 5 :-: Empty `shouldBe` ((:-:) 4 ((:-:) 5 Empty)) -}
        it "can express a binary search tree" $ do
            pending
            {- let nums = [8,6,4,1,7,3,5] -}
            {- let numsTree = foldr treeInsert EmptyTree nums -}
            {- treeElem 5 numsTree `shouldBe` True -}
            {- treeElem 2 numsTree `shouldBe` False -}
