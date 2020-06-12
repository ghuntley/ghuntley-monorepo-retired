import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map
import Data.Char

phoneBook = 
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

main :: IO()
main = hspec $ do
    describe "Map module" $ do
        it "can convert a list to map with Map.fromList" $ do
            pending
            {- ____  ___ `shouldBe` 6 -}
        it "can lookup a value by a key" $ do
            pending
            {- (___ $ ___ phoneBook) `shouldBe` Just "853-2492" -}
            {- (___ "penny1" $ ___ phoneBook) `shouldBe` Nothing -}
        it "returns 0 for size of empty map" $ do
            pending
            {- ___ Map.empty `shouldBe` 0 -}
        it "can insert items into a map" $ do
            pending
            {- (___ $ ___ 3 100 Map.empty) `shouldBe` 1 -}
        it "can check if a Map is empty with null fn" $ do
            pending
            {- ___ Map.empty `shouldBe` True -}
            {- (___ $ Map.fromList [(2,3),(5,5)]) `shouldBe` False -}
        it "can tell if a key is member of a map" $ do
            pending
            {- (___ 3 $ Map.fromList [(3,6),(4,3),(6,9)]) `shouldBe` True -}
            {- (___ 3 $ Map.fromList [(4,3),(6,9)]) `shouldBe` False -}
        it "can map and filter values in a map" $ do
            pending
            {- (___ ___ $ Map.fromList [(1,1),(2,4),(3,9)]) -}
                {- `shouldBe` (Map.fromList [(1,100),(2,400),(3,900)]) -}
            {- (___ ___ $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]) -}
                {- `shouldBe` (Map.fromList [(2,'A'),(4,'B')]) -}
        it "can convert a Map to List" $ do
            pending
            {- Map.singleton initialize a map with a single value -}
            {- Map.insert adds 1 item to it -}
            {- Map.toList converts the map to List -}
            {- (___ . ___ 9 2 $ ___ 4 3) -}
                {- `shouldBe` [(4,3),(9,2)] -}
