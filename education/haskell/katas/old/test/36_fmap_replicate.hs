import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Either

main :: IO ()
main = hspec $ do
    describe "fmap replicate on various types" $ do
        it "can be used over list of ints" $ do
            pending
            {- fmap (replicate _) [___] -}
                {- `shouldBe` [[1,1,1],[2,2,2],[3,3,3]] -}
        it "can be used on Maybe values" $ do
            pending
            {- fmap (replicate _) (Just _) -}
                {- `shouldBe` Just [4,4,4] -}
        it "can fmap on Nothing" $ do
            pending
            {- (fmap (replicate _) Nothing :: Maybe [Int]) -}
                {- `shouldBe` Nothing -}
        it "can be used on Either" $ do
            pending
            {- (fmap (replicate _) (Right ___) :: Either String [String]) -}
                {- `shouldBe` Right ["blah","blah","blah"] -}
            {- (fmap (replicate _) (Left ___) :: Either String String) -}
                {- `shouldBe` Left "blah" -}
