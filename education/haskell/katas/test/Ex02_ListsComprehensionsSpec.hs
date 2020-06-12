module Ex02_ListsComprehensionsSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "List Comprehensions" $ do
        it "squares numbers from 1-5" $ do
            [x^2 | x<-[1..5]] `shouldBe` [1,4,9,16,25]
            {- The solution is: [x^2 | x <- [1..5]]  -}
        it "squares numbers from 1-5 and filters for even" $ do
            [x^2 | x<-[2,4]] `shouldBe` [4,16]
        it "can raise the numbers from 1-5 to the power of 2 and 3" $ do
            [x^y|x<-[1..5],y<-[2,3]] `shouldBe` [1,1,4,8,9,27,16,64,25,125]
        it "can filter the previous list to numbers below 50" $ do
            [x^y|x<-[1..5],y<-[2,3],x^y<50] `shouldBe` [1,1,4,8,9,27,16,25]
        it "can create tuples from lists" $ do
            [(x,y)|x<-[1..3],y<-['a','b']] `shouldBe` [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
        it "can filter for upper-case letters from \"Apple Brick Cat\"" $ do
            [x|x<-"Apple Brick Cat",elem x ['A'..'Z']] `shouldBe` "ABC"
        it "can be generalized into a function" $ do
            let f xs = filter (flip elem ['A'..'Z']) xs
            f "Apple Brick Cat" `shouldBe` "ABC"
