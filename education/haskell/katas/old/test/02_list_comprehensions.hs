import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "List Comprehensions" $ do
        it "squares numbers from 1-5" $ do
            pending
            [] `shouldBe` [1,4,9,16,25]
            {- The solution is: [x^2 | x <- [1..5]]  -}
        it "squares numbers from 1-5 and filters for even" $ do
            pending
            [] `shouldBe` [4,16]
        it "can raise the numbers from 1-5 to the power of 2 and 3" $ do
            pending
            [] `shouldBe` [1,1,4,8,9,27,16,64,25,125]
        it "can filter the previous list to numbers below 50" $ do
            pending
            [] `shouldBe` [1,1,4,8,9,27,16,25]
        it "can create tuples from lists" $ do
            pending
            [] `shouldBe` [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
        it "can filter for upper-case letters from \"Apple Brick Cat\"" $ do
            pending
            [] `shouldBe` "ABC"
        it "can be generalized into a function" $ do
            pending
            let f xs = []
            f "Apple Brick Cat" `shouldBe` "ABC"
