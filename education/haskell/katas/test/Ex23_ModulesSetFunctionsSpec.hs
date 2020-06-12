{-# LANGUAGE UnicodeSyntax #-}

module Ex23_ModulesSetFunctionsSpec
  ( spec
  ) where


import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as Set
import Data.List.Unicode
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Set.Unicode

main :: IO ()
main = hspec spec

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
set1 = Set.fromList text1
set2 = Set.fromList text2

spec :: Spec
spec = do
    describe "Set" $ do
        it "can convert List to Set" $ do
            
            set1 `shouldBe` Set.fromList " .?AIRadefhijlmnorstuy"
            set2 `shouldBe` Set.fromList " !Tabcdefglhilmnorstuvwy"
        it "can find the intersection" $ do
            
             Set.intersection set1 set2
                 `shouldBe` Set.fromList " adefhilmnorstuy"
        it "can find the difference" $ do
            
             Set.difference set1 set2
                 `shouldBe` Set.fromList ".?AIRj"
             Set.difference set2 set1
                 `shouldBe` Set.fromList "!Tbcgvw"
        it "can calculate the union" $ do
          Set.union set1 set2
                 `shouldBe` Set.fromList " !.?AIRTabcdefghijlmnorstuvwy"
        it "can use null, size, singleton, insert, delete" $ do
             Set.null Set.empty `shouldBe` True
             Set.null (Set.fromList [1,2,3]) `shouldBe` False
             Set.size Set.empty `shouldBe` 0
             Set.size (Set.fromList [1,2,3]) `shouldBe` 3
             Set.singleton 9 `shouldBe` Set.fromList [9]
             (Set.insert 8 $ Set.fromList [1..4])
                 `shouldBe` Set.fromList [1,2,3,4,8]
             (Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5])
                 `shouldBe` Set.fromList [3,5]
        it "can filter odd items from set" $ do
             (Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4])
                 `shouldBe` Set.fromList [3,5,7]
