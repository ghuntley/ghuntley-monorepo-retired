import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map

phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

{- inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool -}

{-
 Make sure you understand the difference between:
    * type constructors
    * value constructors
-}

{- LockerState has two value constructors: Taken and Free -}
{- Code is String -}
{- LockerMap is a Map with Int keys and (LockerState, Code) -}

{- lockerLookup :: Int -> LockerMap -> Either String Code -}

{- lockers :: LockerMap -}
{- lockers = Map.fromList -}
    {- [(100,(Taken,"ZD39I")) -}
    {- ,(101,(Free,"JAH3I")) -}
    {- ,(103,(Free,"IQSA9")) -}
    {- ,(105,(Free,"QOTSA")) -}
    {- ,(109,(Free,"893JJ")) -}
    {- ,(110,(Free,"99292")) -}
    {- ] -}

main :: IO()
main = hspec $ do
    describe "Type aliasing" $ do
        it "makes the type declaration more clearer" $ do
            pending
            {- inPhoneBook "wendy" "939-8282" phoneBook -}
                {- `shouldBe` True -}
            {- inPhoneBook "wendy1" "939-8282" phoneBook -}
                {- `shouldBe` False -}
        it "can look up a locker" $ do
            pending
            {- lockerLookup 101 lockers -}
                {- `shouldBe` Right "JAH3I" -}
            {- lockerLookup 100 lockers -}
                {- `shouldBe` Left "Locker 100 is already taken!" -}
            {- lockerLookup 102 lockers -}
                {- `shouldBe` Left "Locker number 102 doesn't exist!" -}
            {- lockerLookup 110 lockers -}
                {- `shouldBe` Right "99292" -}
