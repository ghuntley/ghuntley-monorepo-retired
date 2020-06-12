import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

-- https://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html
import Control.Monad.Reader

-- get the env from the Reader
-- return the env and " This is Tom."
{- tom :: Reader String String -}

-- same here, get the env from Reader
-- return the env and " This is Jerry."
{- jerry :: Reader String String -}

-- call tom and jerry, return their result separate with "\n"
{- tomAndJerry :: Reader String String -}

-- entry function, call it with "Who is this?"
{- runJerryRun :: String -}

main :: IO()
main = hspec $ do
    describe "Monad Reader" $ do
        it "can decuple function from global info" $ do
            pending
            {- let result = "Who is this? This is Tom.\nWho is this? This is Jerry." -}
            {- runJerryRun `shouldBe` result -}
