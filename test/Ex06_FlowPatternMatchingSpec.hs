module Ex06_FlowPatternMatchingSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

-- factorial
factorial :: (Eq p, Num p) => p -> p
-- factorial :: Int -> Int
factorial 1 = 1
factorial x = x * (factorial (x-1))

charName :: Char -> [Char]
charName 'a' = "Albert"
charName _ = "What did you mean?"

addVectors :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- adddVectors x y = (fst x + fst y, snd x + snd y)
addVectors(x1,y1) (x2,y2) = (x1 + x2, y1+y2)

first :: (Int,String,String) -> Int
first (x,_,_) = x

head' :: [a] -> a
-- head' [] = 0
head' (x:_) = x

tell :: [a] -> String
tell [] = "This list is empty"
tell _ = "This list is NOT empty"

spec :: Spec
spec = do
  describe "Pattern matching" $ do
    it "can be used in factorial calc" $ do
      factorial 5 `shouldBe` 120
    it "can fail when no default case" $ do
      charName 'a' `shouldBe` "Albert"
      charName '?' `shouldBe` "What did you mean?"
      -- charName 'd' `shouldThrow` PatternMatchFail
    it "can be used on tuples" $ do
      addVectors (1,2)(3,4) `shouldBe` (4,6)
    it "can be used on triples" $ do
      first (1,"hello","world") `shouldBe` 1
      -- second (1,2,3) `shouldBe` 2
      -- third (1,2,3) `shouldBe` 3
    it "can pattern list comprehensions" $ do
      let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
      [a+b | (a,b) <- xs] `shouldBe` [4,7,6,8,11,4]
    it "can be used for the head function" $ do
      head' [2,3,4] `shouldBe` 2
      head' "Hello" `shouldBe` 'H'
    it "can safely process a list" $ do
      tell [] `shouldBe` "This list is empty"
      -- tell [1] `shouldBe` "This list has one element: 1"
      -- tell [1,2] `shouldBe` "This list has two elements: 1 and 2"
      -- tell [1,2,3] `shouldBe` "This list is too long"
    it "can count elements in list with recursion" $ do
      pending
      -- length' [] `shouldBe` 0
      -- length' [1,2,3] `shouldBe` 3
    it "can reduce add a list" $ do
      pending
      -- sum' [] `shouldBe` 0
      -- sum' [1,2,3] `shouldBe` 6
    it "can hold the original item with pattern" $ do
      pending
      -- firstLetter "" `shouldBe` "Empty string, whoops!"
      -- firstLetter "Dracula" `shouldBe` "The first letter of Dracula is D"
