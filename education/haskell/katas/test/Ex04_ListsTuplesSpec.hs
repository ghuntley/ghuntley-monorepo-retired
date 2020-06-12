module Ex04_ListsTuplesSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "tuples" $ do
        it "can select tuple's first value" $ do

            fst (1,0) `shouldBe` 1
        it "can select tuple's second value" $ do
            snd (True,False) `shouldBe` False
        it "zip can produce tuple pairs" $ do
            zip [1..3] [4..6] `shouldBe` [(1,4),(2,5),(3,6)]
            zip [1..3] ["one","two","three"] `shouldBe` [(1,"one"),(2,"two"),(3,"three")]
            zip [1..3] ["apple","orange","cherry"] `shouldBe` [(1,"apple"),(2,"orange"),(3,"cherry")]
        it "can calculate right triangle that has the perimeter of 24" $ do
            let triangles = [(x,y,z)| x<-[1..10],y<-[1..10],
                                      z<-[1..10],z^2==x^2+y^2,x<y,y<z,24==x+y+z]
            triangles `shouldBe` [(6,8,10)]
