import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Char as C
import qualified Data.List as L
import Data.Function

main :: IO()
main = hspec $ do
    describe "Char functions" $ do
        it "can check if string isAlphaNum" $ do
            pending
            {- ___ _____ "bobby283" `shouldBe` True -}
            {- ___ _____ "some\n" `shouldBe` False -}
        it "can use isSpace to simulate List's words function" $ do
            pending
            {- use groupBy with C.isSpace -}
            {- ___ ______ "hey guys it's me" -}
                {- `shouldBe` ["hey"," ","guys"," ","it's"," ","me"] -}
        it "can filter out spaces from the previous example" $ do
            pending
            {- remove the spaces  -}
            {- ___ _____ $ "hey guys it's me") -}
                {- `shouldBe` ["hey","guys","it's","me"] -}
        it "can categorize characters with generalCategory" $ do
            pending
            {- ___ ' ' `shouldBe` C.Space -}
            {- ___  'U' `shouldBe` C.UppercaseLetter -}
            {- ___ 'z' `shouldBe` C.LowercaseLetter -}
            {- ___ " \t\nA9" -}
                {- `shouldBe` [C.Space,C.Control,C.Control,C.UppercaseLetter,C.DecimalNumber] -}
        it "'ord' and 'chr' convert chars back and forth" $ do
            pending
            {- ___ 'a' `shouldBe` 97 -}
            {- ___ 97 `shouldBe` 'a' -}
        it "can encode a string by shifting its value with provided num" $ do
            pending
            {- encode 3 "Heey" `shouldBe` "Khh|" -}
            {- (decode 3 $ encode 3 "Heeeeey") `shouldBe` "Heeeeey" -}
