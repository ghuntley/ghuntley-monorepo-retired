import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{- cylinder :: (RealFloat a) => a -> a -> a -}
{- cylinder r h = -}
    {- ... -}
    {- ... -}
    {- in sideArea + 2 * topArea -}

{- cylinder' :: (RealFloat a) => a -> a -> a -}
{- cylinder' r h = sideArea + 2 * topArea -}
    {- where ... -}
          {- ... -}

{- calcBmis :: (RealFloat a) => [(a,a)] -> [a] -}

main :: IO()
main = hspec $ do
    describe "Let bindings are almost like where" $ do
        it "can calculate cylinder surface area" $ do
            pending
            {- cylinder 1 2 `shouldBe` 18.84955592153876 -}
            {- cylinder' 1 2 `shouldBe` 18.84955592153876 -}
        it "is an expression in itself" $ do
            pending
            {- let result = [... in (square 5,square 3,square 2)] -}
            {- result `shouldBe` [(25,9,4)] -}
        it "can be used in list comprehensions" $ do
            pending
            {- calcBmis [(80,1.9)] `shouldBe` [] -}

