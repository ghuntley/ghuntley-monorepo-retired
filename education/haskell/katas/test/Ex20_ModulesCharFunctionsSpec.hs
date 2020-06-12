{-# LANGUAGE UnicodeSyntax #-}

module Ex20_ModulesCharFunctionsSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Char as C
import qualified Data.List as L
import Data.Function
import Data.List.Unicode
import Prelude.Unicode
import Data.Monoid.Unicode


encode shift msg = L.map (C.chr . (+ shift) . C.ord) msg
decode shift txt = L.map (C.chr ∘ (+ (-shift)) ∘ C.ord) txt

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Char functions" $ do
        it "can check if string isAlphaNum" $ do
             all C.isAlphaNum "bobby283" `shouldBe` True
             all C.isAlphaNum "some\n" `shouldBe` False
        it "can use isSpace to simulate List's words function" $ do
            {- use groupBy with C.isSpace -}
          (L.groupBy ((≡) `on` C.isSpace) "hey guys it's me")
                 `shouldBe` ["hey"," ","guys"," ","it's"," ","me"]
        it "can filter out spaces from the previous example" $ do
            {- remove the spaces  -}
             (L.filter (not ∘ any C.isSpace) . L.groupBy (on (≡) C.isSpace) $ "hey guys it's me")
                 `shouldBe` ["hey","guys","it's","me"]
        it "can categorize characters with generalCategory" $ do
             (C.generalCategory ' ') `shouldBe` C.Space
             (C.generalCategory  'U') `shouldBe` C.UppercaseLetter
             (C.generalCategory 'z') `shouldBe` C.LowercaseLetter
             (map C.generalCategory " \t\nA9")
                 `shouldBe` [C.Space,C.Control,C.Control,C.UppercaseLetter,C.DecimalNumber]
        it "'ord' and 'chr' convert chars back and forth" $ do
            C.ord 'a' `shouldBe` 97
            C.chr 97 `shouldBe` 'a'
        it "can encode a string by shifting its value with provided num" $ do
             encode 3 "Heey" `shouldBe` "Khh|"
             (decode 3 $ encode 3 "Heeeeey") `shouldBe` "Heeeeey"
