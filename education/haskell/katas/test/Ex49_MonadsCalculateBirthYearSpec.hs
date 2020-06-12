{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ex49_MonadsCalculateBirthYearSpec
  ( spec
  ) where

import Test.Hspec
import Text.Read (readMaybe)
import Data.Monoid.Unicode
import Prelude.Unicode
import Data.Monoid.Unicode

{-
    Calculate how old will someone be by the year 2020
    Use just `read` to convert String -> Int
-}

main :: IO ()
main = hspec spec

calculateAge :: String -> Int
calculateAge year =
    if year == "" then 0 else 2020 - read year
    
-- when nothing - "Provided invalid year"
-- when value - "In 2020 you'll be x"
displayAge :: Maybe Int -> String
displayAge maybeAge =
  case maybeAge of
    Nothing -> "Provided invalid year"
    Just age -> "In 2020 you'll be " ++ show age

-- uses readMaybe to parse the string
-- calls yearToAge for real value
convertAgeFromString :: String -> Maybe Int
convertAgeFromString y =
  case readMaybe y of
    Nothing -> Nothing
    Just birthYear -> Just (yearToAge birthYear)

yearToAge :: Int -> Int
yearToAge year = 2020 - year

spec :: Spec
spec = do
    describe "Calculate Birth Year 01" $ do
        it "calculates age with read" $ do
             birthYear <- pure "1980"
             (2020 - read birthYear) `shouldBe` 40
        it "calculates age with readMaybe" $ do
            -- calculateAge should use case of
             birthYear <- pure "1980"
             calculateAge birthYear `shouldBe` 40
             birthYear <- pure ""
             calculateAge birthYear `shouldBe` 0
        it "uses separate functions" $ do
             birthYear <- pure "1980"
             let maybeAge = convertAgeFromString birthYear
             displayAge maybeAge `shouldBe` "In 2020 you'll be 40"
             birthYear <- pure ""
             let maybeAge = convertAgeFromString birthYear
             displayAge maybeAge `shouldBe` "Provided invalid year"
        it "uses fmap, no need for convert fn" $ do
             birthYear <- pure "1980"
             let maybeAge = fmap yearToAge (readMaybe birthYear)
             displayAge maybeAge `shouldBe` "In 2020 you'll be 40"
             birthYear <- pure ""
             let maybeAge = convertAgeFromString birthYear
             displayAge maybeAge `shouldBe` "Provided invalid year"
        it "works with do notation" $ do
          pending
             -- birthYear <- pure "1980"
             -- let maybeAge = do
             --     yearInteger <- readMaybe birthYear
             --     return $ yearToAge yearInteger
             -- displayAge maybeAge `shouldBe` "In 2020 you'll be 40"





