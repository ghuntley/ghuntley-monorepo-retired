import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "ranges" $ do
        it "can express ranges" $ do
            pending
            length [] `shouldBe` 10
        it "can set steps" $ do
            pending
            take 3 [] `shouldBe` [2,4,6]
    describe "cycle" $ do
        it "can cycle through numbers" $ do
            pending
            (cycle []) `shouldBe` [1,2,3,1,2]
    describe "repeat" $ do
        it "can repeat numbers" $ do
            pending
            take 1 [] `shouldBe` [3,3,3,3,3]
    describe "elem" $ do
        it "can remove non-uppercase letters" $ do
            pending
            let removeNonUppercase st = []
            removeNonUppercase "IdontLIKEFROGS" `shouldBe` "ILIKEFROGS"
