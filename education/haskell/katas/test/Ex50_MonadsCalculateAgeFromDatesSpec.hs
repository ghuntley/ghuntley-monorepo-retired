
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ex50_MonadsCalculateAgeFromDatesSpec
  ( spec
  ) where

-- From: http://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads
-- 6. Using the short-hand <$>
import Test.Hspec
import Text.Read (readMaybe)
import Data.Monoid.Unicode
import Prelude.Unicode


main :: IO ()
main = hspec spec

displayAge :: Maybe Int -> String
displayAge maybeAge =
    case maybeAge of
         Nothing -> "Provided invalid year"
         Just age -> "In that year you'll be " ++ show age

yearDiff :: Int -> Int -> Int
yearDiff futureYear birthYear = futureYear - birthYear

-- use only case statements
withReadMaybes :: String -> String -> Maybe Int
withReadMaybes bys fys =
  let maybeAge = do
        mby <- readMaybe bys
        mfy <- readMaybe fys
        return $ mfy - mby
   in maybeAge


spec :: Spec
spec = do
    describe "Calculate age from two dates" $ do
        it "calculates age with readMaybes" $ do
            pending
            {- birthYearString <- pure "1980" -}
            {- futureYearString <- pure "2021" -}
            {- let maybeAge = withReadMaybes birthYearString futureYearString -}
            {- displayAge maybeAge `shouldBe` "In that year you'll be 41" -}
        it "calculates age with do notation" $ do
            pending
            {- birthYearString <- pure "1980" -}
            {- futureYearString <- pure "2021" -}
            {- let maybeAge = do -}
                {- birthYear <- ___ -}
                {- futureYear <- ___ -}
                {- return $ ___ futureYear birthYear -}
            {- displayAge maybeAge `shouldBe` "In that year you'll be 41" -}
        it "calculates age with fmap" $ do
            pending
            {- birthYearString <- pure "1980" -}
            {- futureYearString <- pure "2021" -}
            {- let maybeAge = do -}
                    {- yearToAge <- ___ yearDiff (___ ___) -}
                    {- birthYear <- ___ ___ -}
                    {- return $ yearToAge birthYear -}
            {- displayAge maybeAge `shouldBe` "In that year you'll be 41" -}
        it "calculates age with two fmaps" $ do
            pending
            {- birthYearString <- pure "1980" -}
            {- futureYearString <- pure "2021" -}
            {- let maybeAge = do -}
                    {- yearToAge <- ___ yearDiff (___ ___) -}
                    {- ___ yearToAge (___ ___) -}
            {- displayAge maybeAge `shouldBe` "In that year you'll be 41" -}
        it "calculates age with functor" $ do
            pending
            {- birthYearString <- pure "1980" -}
            {- futureYearString <- pure "2021" -}
            {- let maybeAge = do -}
                    {- ___ yearDiff (___ ___) -}
                    {- ___ readMaybe ___ -}
            {- displayAge maybeAge `shouldBe` "In that year you'll be 41" -}
        it "calculates age with functors and applicative" $ do
            pending
            {- birthYearString <- pure "1980" -}
            {- futureYearString <- pure "2021" -}
            {- let maybeAge = yearDiff -}
                    {- ___ readMaybe ___ -}
                    {- ___ readMaybe ___ -}
            {- displayAge maybeAge `shouldBe` "In that year you'll be 41" -}
        it "can autocorrect the calculation" $ do
            pending
            {- birthYearString <- pure "2021" -}
            {- futureYearString <- pure "1980" -}
            {- let maybeAge = do -}
                    {- futureYear <- readMaybe futureYearString -}
                    {- birthYear <- readMaybe birthYearString -}
                    -- check if the futureYear is less than birthyear, autocorrect
                    {- return $ -}
                        {- _____ -}
                        {- _____ -}
                        {- _____ -}
            {- displayAge maybeAge `shouldBe` "In that year you'll be 41" -}
