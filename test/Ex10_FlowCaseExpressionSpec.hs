module Ex10_FlowCaseExpressionSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec
    {- ___ -}
    {- ___ -}
    {- where ___ [] = "is empty." -}
          {- ___ -}
          {- ___ -}

head' :: [a] -> a
head' xs = case xs of
             [] -> error "Empty list"
             (x:_) -> x

-- Alternate method below to above
-- head' xs = if null xs then error "Empty List" else let myHead (x:_) = x in myHead xs

describeList :: [a] -> String
describeList xs =
  case xs of
    [] -> "The list is empty."
    [x] -> "The list is a singleton list."
    (_:_) -> "The list is a longer list."
-- describeList xs = "The list is " ++ case xs ___
-- Case statement can be written with patten matching
-- describeList xs = "The list is " ++ what xs

spec :: Spec
spec =
  describe "Case expressions" $ do
    it "can be used anywhere" $ do
      head' [1,3] `shouldBe` 1
    it "can be even used in expressions" $ do
      describeList [] `shouldBe` "The list is empty."
      describeList [1] `shouldBe` "The list is a singleton list."
      describeList [1,2] `shouldBe` "The list is a longer list."
