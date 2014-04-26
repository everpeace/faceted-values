module Data.FacetedSpec where

import Control.Applicative
import Data.Faceted
import Test.Hspec

-- Simple Faceted Value
simple = (\x -> x > 0) ? 1 .: 0
nested = (\x -> x <= 2) ?? ((\x -> x < 2) ? 1 .: 2) .: ((\x -> x < 4 ) ? 3 .: 4)
-- do syntax test cases
ap_do = do a <- ((\x -> 0 < x && x < 3) ? 1 .: 2)
           b <- ((\x -> 1 < x && x < 4) ? 4 .: 8)
           return (a + b)
monad_do = do a <- ((\x -> 0 < x && x < 3) ? 1 .: 2)
              b <- ((\x -> 1 < x && x < 4) ? (a+4) .: (a+8))
              (\y -> y < 3) ? (10*b) .: (100*b)

spec :: Spec
spec = do
  describe "facete value can be declared in intuitive manner \"(\\x -> x > 0) ? 1 .: 0)\"" $ do
    it "its observation with context 0 should be 0." $
      observe simple 0 `shouldBe` 0
    it "its observation with context 1 should be 1." $
      observe simple 1 `shouldBe` 1


  describe "use (??) for nested facete value \"(\\x -> x <= 2) ?? ((\\x -> x < 2) ? 1 .: 2) .: ((\\x -> x < 4 ) ? 3 .: 4)\"" $ do
    it "its observation with context 1 should be 1." $
      observe nested 1 `shouldBe` 1
    it "its observation with context 2 should be 2." $
      observe nested 2 `shouldBe` 2
    it "its observation with context 3 should be 3." $
      observe nested 3 `shouldBe` 3
    it "its observation with context 4 should be 4." $
      observe nested 4 `shouldBe` 4

  describe "Functor: ((*3) `fmap` (\\x -> x > 0) ? 1 .: 0)) should be equivalent with < (x > 0) ? 1*3 : 0*3>." $ do
    it "observation with context 0 should be 0." $
      observe ((*3) `fmap` simple) 0 `shouldBe` 0
    it "observation with context 1 should be 3." $
      observe ((*3) `fmap` simple) 1 `shouldBe` 3


  describe ("Applicative: ((+) <$> ((\\x -> 0 < x && x < 3) ? 1 .: 2) <*> ((\\x -> 1 < x && x < 4) ? 4 .: 8))\n"
          ++"\tThis computation adds two faceted values. So in this case, 4 patterns of results can be observed.\n"
          ++"\tThe result should be equivalent with\n"
          ++"\t  < (0 < x < 3) ? < (1 < x < 4) ? 1+4 : 1+8 >\n"
          ++"\t                : < (1 < x < 4) ? 2+4 : 2+8 >>") $ do
    it "observation with context 1 should be 9  (= 1 + 8)." $
      observe ( (+) <$> ((\x -> 0 < x && x < 3) ? 1 .: 2) <*> ((\x -> 1 < x && x < 4) ? 4 .: 8)) 1 `shouldBe` 9
    it "observation with context 2 should be 5  (= 1 + 4)." $
      observe ( (+) <$> ((\x -> 0 < x && x < 3) ? 1 .: 2) <*> ((\x -> 1 < x && x < 4) ? 4 .: 8)) 2 `shouldBe` 5
    it "observation with context 3 should be 6  (= 2 + 4)." $
      observe ( (+) <$> ((\x -> 0 < x && x < 3) ? 1 .: 2) <*> ((\x -> 1 < x && x < 4) ? 4 .: 8)) 3 `shouldBe` 6
    it "observation with context 4 should be 10 (= 2 + 8)." $
      observe ( (+) <$> ((\x -> 0 < x && x < 3) ? 1 .: 2) <*> ((\x -> 1 < x && x < 4) ? 4 .: 8)) 4 `shouldBe` 10

  describe ("Applicative Do: \n"
          ++"\t do a <- ((\\x -> 0 < x && x < 3) ? 1 .: 2)\n"
          ++"\t    b <- ((\\x -> 1 < x && x < 4) ? 4 .: 8)\n"
          ++"\t    return a + b\n"
          ++"\tshould be equivalent with above.\n") $ do
    it "observation with context 1 should be 9  (= 1 + 8)." $
      observe ap_do 1 `shouldBe` 9
    it "observation with context 2 should be 5  (= 1 + 4)." $
      observe ap_do 2 `shouldBe` 5
    it "observation with context 3 should be 6  (= 2 + 4)." $
      observe ap_do 3 `shouldBe` 6
    it "observation with context 4 should be 10 (= 2 + 8)." $
      observe ap_do 4 `shouldBe` 10


  describe ("Bind: ((\\x -> 0 < x && x < 3) ? 1 .: 2) >>= (\\v -> ((\\x -> 1 < x && x < 4) ? (v+4) .: (v+8))\n"
          ++"\tshoule be equivalent with\n"
          ++"\t  < (0 < x < 3) ? < (1 < x < 4) ? 1+4 : 1+8 >\n"
          ++"\t                : < (1 < x < 4) ? 2+4 : 2+8 >>") $ do
    it "observation with context 1 should be 9  (= 1 + 8)." $
      observe (((\x -> 0 < x && x < 3) ? 1 .: 2) >>= \v -> ((\x -> 1 < x && x < 4) ? (v+4) .: (v+8))) 1 `shouldBe` 9
    it "observation with context 2 should be 5  (= 1 + 4)." $
      observe (((\x -> 0 < x && x < 3) ? 1 .: 2) >>= \v -> ((\x -> 1 < x && x < 4) ? (v+4) .: (v+8))) 2 `shouldBe` 5
    it "observation with context 3 should be 6  (= 2 + 4)." $
      observe (((\x -> 0 < x && x < 3) ? 1 .: 2) >>= \v -> ((\x -> 1 < x && x < 4) ? (v+4) .: (v+8))) 3 `shouldBe` 6
    it "observation with context 4 should be 10 (= 2 + 8)." $
      observe (((\x -> 0 < x && x < 3) ? 1 .: 2) >>= \v -> ((\x -> 1 < x && x < 4) ? (v+4) .: (v+8))) 4 `shouldBe` 10

  describe ("Do Syntax:\n"
          ++"\tdo a <- ((\\x -> 0 < x && x < 3) ? 1 .: 2)\n"
          ++"\t   b <- ((\\x -> 1 < x && x < 4) ? (a+4) .: (a+8))\n"
          ++"\t   (\\y -> y < 3) ? (10*b) .: (100*b)\n"
          ++"\tshoule be equivalent with\n"
          ++"\t  < (0 < x < 3) ? < (1 < x < 4) ? <(y < 3)? 10*(1+4) : 100*(1+4)>\n"
          ++"\t                                : <(y < 3)? 10*(2+8) : 100*(2+8)>>\n"
          ++"\t                : < (1 < x < 4) ? <(y < 3)? 10*(2+4) : 100*(2+4)>\n"
          ++"\t                                : <(y < 3)? 10*(2+8) : 100*(2+8)>>") $ do
    it "observation with context 1 should be 90   (= 10 * (1 + 8))." $
      observe monad_do 1 `shouldBe` 90
    it "observation with context 2 should be 50   (= 10 * (1 + 4))." $
      observe monad_do 2 `shouldBe` 50
    it "observation with context 3 should be 600  (= 100 * (2 + 4))." $
      observe monad_do 3 `shouldBe` 600
    it "observation with context 4 should be 1000 (= 100 * (2 + 8))." $
      observe monad_do 4 `shouldBe` 1000
