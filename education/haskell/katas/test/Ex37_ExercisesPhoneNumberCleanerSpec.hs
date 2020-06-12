{-# LANGUAGE UnicodeSyntax #-}
module Ex37_ExercisesPhoneNumberCleanerSpec
  ( spec
  ) where

import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Map.Unicode
import Data.Sequence.Unicode


main :: IO ()
main = hspec spec

convert :: [String] -> [(Int, String)]
convert pns =     
  let
    makeTuples pns = zipWith (\x y -> (y, x)) pns [0..]
    onlyNumericChars pns = [x | x<-pns, x `elem` ['0'..'9']]
    filterNon10s pnst = filter (\(idx, pn) -> length pn == 10) pnst
  in
    filterNon10s $ makeTuples $ map onlyNumericChars pns

spec :: Spec
spec =
    describe "Phone Number Cleaner" $ do
        let result = [(0,"3128342494")
                     ,(1,"2183422284")]
        it "can convert to a tuple with positions" $ do
            let input = ["3128342494"
                        ,"2183422284"]
            convert input `shouldBe` result
        it "can filter out non-numeric chars" $ do
            let input = ["a3128342494"
                        ,"218b3422284"]
            convert input `shouldBe` result
        it "can filter out every non-10 digit numbers" $ do
            let input = ["3128342494"
                        ,"2183422284"
                        ,"123"]
            convert input `shouldBe` result
        it "can filter out invalid numbers mixed with other chars" $ do
            let input = ["31283424"
                        ,"2183422284"
                        ,"12345678901"
                        ,"3123528941"
                        ,"312-284-3919"
                        ,"313.284.3941"
                        ,"bb312"
                        ,"a338-423|4391x"]
            let result = [(1,"2183422284")
                         ,(3,"3123528941")
                         ,(4,"3122843919")
                         ,(5,"3132843941")
                         ,(7,"3384234391")]
            convert input `shouldBe` result
