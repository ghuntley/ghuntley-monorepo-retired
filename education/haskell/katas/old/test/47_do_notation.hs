import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


{- foo :: Maybe String -}
{- foo = ___   >>= (\x -> -}
      {- ___ >>= (\y -> -}
      {- ___ )) -}

-- Use do expression
{- fooMonad :: Maybe String -}

-- Use do expression here as well
{- marySue :: Maybe Bool -}

main :: IO()
main = hspec $ do
    describe "The do notation" $ do
        it "can combine functions with Maybe values" $ do
            pending
            -- Use a lambda here
            {- (Just 3 >>= (___)) -}
                {- `shouldBe` Just "3!" -}
            {- (Just 3 >>= (\x -> ___  >>= (\y -> Just ___))) -}
                {- `shouldBe` Just "3!" -}
            {- (Just 3 >>= (\x -> Just "!" >>= (\y -> ___))) -}
                {- `shouldBe` (Nothing :: Maybe String) -}
        it "can be wrapped on different lines" $ do
            pending
            {- foo `shouldBe` Just "3!" -}
            {- fooMonad `shouldBe` Just "3!" -}
        it "can simplify more complex expressions" $ do
            pending
            -- Use a lambda here for the prdicate
            {- (Just 9 >>= (___)) `shouldBe` Just True -}
            {- marySue `shouldBe` Just True -}
