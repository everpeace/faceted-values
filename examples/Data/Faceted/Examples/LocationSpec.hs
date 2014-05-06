module Data.Faceted.Examples.LocationSpec where

import Text.Show.Functions
import Data.Faceted
import Test.Hspec

-- User Data
data User = User { name:: String, company:: String, location:: (Int, Int)}
alice = User "alice" "PrivateCorp" (123, 456)
bob   = User "bob"   "PrivateCorp" (234, 567)
carl  = User "carl"  "PublicCorp"  (456, 789)

-- Policies
isMe :: User -> User -> Bool
isMe me observer = (name observer) == (name me)

isCoWorker :: User -> User -> Bool
isCoWorker me observer = (company observer) == (company me)

-- locationOf is a faceted value.
-- < me ?? locaiton : < coworker ? locaiton roounded by 10 : location rounded by 100> >
locationOf :: User -> Faceted User (Int, Int)
locationOf user = (isMe user) ?? constF (location user)
                              .: ((isCoWorker user) ? (x - (x `mod` 10), y - (y `mod` 10))
                                                   .: (x - (x `mod` 100), y - (y `mod` 100)))
  where
    x = fst $ location user
    y = snd $ location user

distance :: Integral a => (a, a) -> (a, a) -> Double
distance (a,b) (c,d) = sqrt $ (a'-c')*(a'-c') + (b'-d')*(b'-d')
  where
    a' = fromIntegral a
    b' = fromIntegral b
    c' = fromIntegral c
    d' = fromIntegral d

distanceTo :: User -> User -> Faceted User Double
distanceTo he she= do locHe <- locationOf he
                      locShe <- locationOf she
                      return (distance locHe locShe)

spec :: Spec
spec = do
  describe "location can varied by observers" $ do
    it "alice's location observed by herself should be (123, 456)" $
      (observe (locationOf alice) alice) `shouldBe` (123, 456)
    it "bob's location observed by himself should be (234, 567)" $
      (observe (locationOf bob) bob) `shouldBe` (234, 567)
    it "carl's location observed by himself should be (456, 789)" $
      (observe (locationOf carl) carl) `shouldBe` (456, 789)
    it "alice's location observed by bob(co-worker) should be (120, 450) (rounded by ten.)" $
      (observe (locationOf alice) bob) `shouldBe` (120, 450)
    it "alice's location observed by carl(not-coworker) should be (100, 400) (rounded by hundred.)" $
      (observe (locationOf alice) carl) `shouldBe` (100, 400)

  describe "distance can be varied by observers" $ do
    it "distance btw alice and bob observed by alice should be d((123,456), (230,560))" $
      (observe (distanceTo alice bob) alice) `shouldBe` (distance (123, 456) (230, 560))
    it "distance btw alice and bob observed by bob should be d((120, 450), (234, 567))" $
      (observe (distanceTo alice bob) bob) `shouldBe` (distance (120, 450) (234, 567))
    it "distance btw alice and bob observed by bob should be d((100, 400), (200, 500))" $
      (observe (distanceTo alice bob) carl) `shouldBe` (distance (100, 400) (200, 500))
