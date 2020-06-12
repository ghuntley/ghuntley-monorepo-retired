import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show, Eq)
type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

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
    in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
          then reverse bestAPath
          else reverse bestBPath


main :: IO()
main = hspec $ do
    describe "London - to - Heathrow" $ do
        it "calculates the road path" $ do
            roadStep ([],[]) (head heathrowToLondon) `shouldBe`
                ([(C,30),(B,10)],[(B,10)])
        it "calculates the best path" $ do
            optimalPath heathrowToLondon `shouldBe`
                [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
