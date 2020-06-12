import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Set as Set

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"
set1 = Set.fromList text1
set2 = Set.fromList text2

main :: IO()
main = hspec $ do
    describe "Set" $ do
        it "can convert List to Set" $ do
            pending
            {- ___ `shouldBe` ___ " .?AIRadefhijlmnorstuy" -}
            {- ___ `shouldBe` ___ " !Tabcdefglhilmnorstuvwy" -}
        it "can find the intersection" $ do
            pending
            {- ___ set1 set2 -}
                {- `shouldBe` Set.fromList " adefhilmnorstuy" -}
        it "can find the difference" $ do
            pending
            {- ___ set1 set2 -}
                {- `shouldBe` Set.fromList ".?AIRj" -}
            {- ___ set2 set1 -}
                {- `shouldBe` Set.fromList "!Tbcgvw" -}
        it "can calculate the union" $ do
            pending
            {- ___ set1 set2 -}
                {- `shouldBe` Set.fromList " !.?AIRTabcdefghijlmnorstuvwy" -}
        it "can use null, size, singleton, insert, delete" $ do
            pending
            {- ___ Set.empty `shouldBe` True -}
            {- ___ (Set.fromList [1,2,3]) `shouldBe` False -}
            {- ___ Set.empty `shouldBe` 0 -}
            {- ___ (Set.fromList [1,2,3]) `shouldBe` 3 -}
            {- ___ 9 `shouldBe` Set.fromList [9] -}
            {- (___ 8 $ Set.fromList [1..4]) -}
                {- `shouldBe` Set.fromList [1,2,3,4,8] -}
            {- (___ 4 $ Set.fromList [3,4,5,4,3,4,5]) -}
                {- `shouldBe` Set.fromList [3,5] -}
        it "can filter odd items from set" $ do
            pending
            {- (___ ___ $ Set.fromList [3,4,5,6,7,2,3,4]) -}
                {- `shouldBe` Set.fromList [3,5,7] -}
