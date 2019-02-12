module Ex08_FlowWhereSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

-- BMI boundaries are 18.5, 25.0 and 30.0
-- Calculation logic: weight / height ^ 2
-- bmiTell weight height
bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal."
    | bmi <= fat = "You're fat! Lose some weight!"
    | otherwise   = "You're a whale, congratulations!"
    where
      skinny = 18.5
      normal = 25.0
      fat = 30.0
      bmi :: Double
      bmi = weight / height ^ 2

initials :: String -> String -> String
initials "" "" = ""
initials (x:_) (y:_) = [x,y]
initials (x:_) "" = [x]

calcBmis :: Fractional t => [(t, t)] -> [t]
calcBmis xs = [w / h ^ 2 | (w,h) <- xs]
  -- where
    -- calcBmis w h = w / h ^ 2


spec :: Spec
spec =
  describe "where - to DRY up logic" $ do
    it "can calculate BMI from values" $ do
      bmiTell 85 1.90 `shouldBe` "You're supposedly normal."
    it "can extract initials from a string" $ do
      initials "" "" `shouldBe` ""
      initials "Attila" "Domokos" `shouldBe` "AD"
    it "can be used in list comprehensions" $ do
      calcBmis [(85, 1.90)] `shouldBe` [23.545706371191137]
