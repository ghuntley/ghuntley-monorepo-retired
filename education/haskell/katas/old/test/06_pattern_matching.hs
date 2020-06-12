import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Pattern matching" $ do
        it "can be used in factorial calc" $ do
            pending
            {- factorial 5 `shouldBe` 120 -}
        it "can fail when no default case" $ do
            pending
            {- charName 'a' `shouldBe` "Albert" -}
            {- charName'd' `shouldThrow` PatternMatchFail -}
        it "can be used on tuples" $ do
            pending
            {- addVectors (1,2)(3,4) `shouldBe` (4,6) -}
        it "can be used on triples" $ do
            pending
            {- first (1,2,3) `shouldBe` 1 -}
            {- second (1,2,3) `shouldBe` 2 -}
            {- third (1,2,3) `shouldBe` 3 -}
        it "can pattern list comprehensions" $ do
            pending
            {- let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)] -}
            {- [a+b | (a,b) <- xs] `shouldBe` [4,7,6,8,11,4] -}
        it "can be used for the head function" $ do
            pending
            {- head' [2,3,4] `shouldBe` 2 -}
            {- head' "Hello" `shouldBe` 'H' -}
        it "can safely process a list" $ do
            pending
            {- tell [] `shouldBe` "This list is empty" -}
            {- tell [1] `shouldBe` "This list has one element: 1" -}
            {- tell [1,2] `shouldBe` "This list has two elements: 1 and 2" -}
            {- tell [1,2,3] `shouldBe` "This list is too long" -}
        it "can count elements in list with recursion" $ do
            pending
            {- length' [] `shouldBe` 0 -}
            {- length' [1,2,3] `shouldBe` 3 -}
        it "can reduce add a list" $ do
            pending
            {- sum' [] `shouldBe` 0 -}
            {- sum' [1,2,3] `shouldBe` 6 -}
        it "can hold the original item with pattern" $ do
            pending
            {- firstLetter "" `shouldBe` "Empty string, whoops!" -}
            {- firstLetter "Dracula" `shouldBe` "The first letter of Dracula is D" -}

