{-# LANGUAGE UnicodeSyntax #-}
module Ex22_ModulesMapFunctionsSpec
  ( spec
  ) where


import Test.Hspec
import Test.QuickCheck
import qualified Data.Map as Map
import Data.Char
import Data.List.Unicode
import Prelude.Unicode
import Data.Monoid.Unicode

main :: IO ()
main = hspec spec

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

spec :: Spec
spec = do
    describe "Map module" $ do
        it "can convert a list to map with Map.fromList" $ do
             (Map.size  $ Map.fromList phoneBook) `shouldBe` 6
        it "can lookup a value by a key" $ do
             (Map.lookup "penny" $ Map.fromList phoneBook) `shouldBe` Just "853-2492"
             (Map.lookup "penny1" $ Map.fromList phoneBook) `shouldBe` Nothing
        it "returns 0 for size of empty map" $ do
             Map.size Map.empty `shouldBe` 0
        it "can insert items into a map" $ do
             (Map.size $ Map.insert 3 100 Map.empty) `shouldBe` 1
        it "can check if a Map is empty with null fn" $ do
             Map.null Map.empty `shouldBe` True
             (Map.null $ Map.fromList [(2,3),(5,5)]) `shouldBe` False
        it "can tell if a key is member of a map" $ do
             (Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]) `shouldBe` True
             (Map.member 3 $ Map.fromList [(4,3),(6,9)]) `shouldBe` False
        it "can map and filter values in a map" $ do
             (Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)])
                 `shouldBe` (Map.fromList [(1,100),(2,400),(3,900)])
             (Map.filter (isUpper) $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')])
                 `shouldBe` (Map.fromList [(2,'A'),(4,'B')])
        it "can convert a Map to List" $ do
            {- Map.singleton initialize a map with a single value -}
            {- Map.insert adds 1 item to it -}
            {- Map.toList converts the map to List -}
             (Map.toList . Map.insert 9 2 $ Map.singleton 4 3)
                 `shouldBe` [(4,3),(9,2)]
