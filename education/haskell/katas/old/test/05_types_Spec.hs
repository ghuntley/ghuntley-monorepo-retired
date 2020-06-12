import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{- Create the `removeNonUppercase` function with proper type -}

{- Create the addThree function with proper type info -}

main :: IO ()
main = hspec $ do
    describe "Functions have types" $ do
        it "can use a function with type" $ do
            pending
            {- removeNonUppercase "HelloWORLD" `shouldBe` "HWORLD" -}
            {- addThree 1 2 3 `shouldBe` 6 -}
    describe "Type classes" $ do
        it "can order strings" $ do
            pending
            "Abrakadabra" == "Zebra" `shouldBe` True
            {- use the words "Abrakadabra"  "Zebra" -}
            {- GT `shouldBe` LT -}
            {-x >= y `shouldBe` True -}
            {- 3 functionName 5 `shouldBe` GT -}
        it "can show anything" $ do
            pending
            {- 3 `shouldBe` "3" -}
            {- True `shouldBe` "True" -}
        it "can read strings into values" $ do
            pending
            {- "True" || False `shouldBe` True -}
            {- "8.2" + 3.8 `shouldBe` 12 -}
            {- ("[1,2,3,4]" :: [Int]) `shouldBe` [1,2,3,4] -}
            {- ("(3, 'a')" :: (Int, Char)) `shouldBe` (3, 'a') -}
        it "can provide ranges, next items for Enum types" $ do
            pending
            {- `shouldBe` "abcde" -}
            {- `shouldBe` [LT,EQ,GT] -}
            {- `shouldBe` [3,4,5] -}
            {- succ to get the next -}
            {- `shouldBe` 'C' -}
    describe "Num is a numeric typeclass" $ do
        it "can act like numbers" $ do
            pending
            {- use the type -}
            {- (20 :: ) `shouldBe` 20 -}
    describe "fromIntegral is there historical reasons" $ do
        it "can add Int and Floating point numbers" $ do
            pending
            {- from... function -}
            {- (length [1,2,3,4]) + 3.2 `shouldBe` 7.2 -}
