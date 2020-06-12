{-# LANGUAGE UnicodeSyntax #-}
module Ex35_ExercisesLondonToHeathrowSpec
  ( spec
  ) where

import Control.Exception (evaluate)
import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Map.Unicode
import Data.Sequence.Unicode

main :: IO ()
main = hspec spec

-- data Node = Node Road Road | EndNode Road
-- data Road = Road Int Node
data Section = Section { getA :: Int, getB :: Int,
                         getC :: Int } deriving (Show)
type RoadSystem = [Section]
data Label = A | B | C deriving (Show, Eq)
type Path = [(Label, Int)]


heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30,
                    Section 5 90 20,
                    Section 40 2 25,
                    Section 10 8 0]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                     then (A,a):pathA  
                     else (C,c):(B,b):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB  
                     then (B,b):pathB  
                     else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)  

 


optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath  
{-
 A 50 (A1) 5  (A2) 40 (A3) 10 (A4)
       30      20      25      0
 B 10 (B1) 90 (B2)  2 (B3)  8 (B4)
-}

spec :: Spec
spec = do
    describe "London - to - Heathrow" $ do
        it "calculates the road path for first" $ do
              roadStep ([],[]) (head heathrowToLondon) `shouldBe`
                 ([(C,30),(B,10)],[(B,10)])
        it "calculates the road path for second" $ do
             roadStep ([],[]) (heathrowToLondon !! 1) `shouldBe`
                 ([(A,5)],[(C,20),(A,5)])
        it "calculates the road path for third" $ do
             roadStep ([],[]) (heathrowToLondon !! 2) `shouldBe`
                 ([(C,25),(B,2)],[(B,2)])
        it "calculates the best path" $ do
             optimalPath heathrowToLondon `shouldBe`
                 [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
