module Data.FacetedSpec where

import Control.Applicative
import Data.Faceted
import Test.Hspec

-- Simple Faceted Value < (\x -> x >= 1) ? 1 : 0>
simple:: Faceted Int Int
simple = Faceted (\x -> x > 0) (1, 0)

-- do syntax test cases
ap_do :: Faceted Int Int
ap_do = do a <- simple
           b <- simple
           return (a+b)
monad_do :: Faceted Int Int
monad_do = do a <- simple
              b <- simple
              Faceted (\y -> y < 2) (a*b, a+b)

spec :: Spec
spec = do
  describe "faceted value simple =  Faceted (\\x -> x > 0) (1, 0) (which is equivalent with < (x > 0) ? 1 : 0>)" $ do
    it "observation with context 1 should be 1" $
      observe simple 1 `shouldBe` 1

    it "observation with context 0 should be 0" $
      observe simple 0 `shouldBe` 0


  describe "Functor: ((*3) `fmap` simple) should be equivalent with < (x > 0) ? 1*3 : 0*3> " $ do
    it "observation with context 1 should be 3" $
      observe ((*3) `fmap` simple) 1 `shouldBe` 3

    it "observation with context 0 should be 0" $
      observe ((*3) `fmap` simple) 0 `shouldBe` 0


  describe "Applicative: ((+) <$> simple <*> simple) should  be equivalent with < (x > 0) ? 1+1 : 0+0>" $ do
    it "observation with context 1 should be 3" $
      observe ((+) <$> simple <*> simple) 1 `shouldBe` 2

    it "observation with context 0 should be 0" $
      observe ((+) <$> simple <*> simple) 0 `shouldBe` 0

  describe ("Applicative Do: \n"
          ++"\t do a <- simple\n"
          ++"\t    b <- simple\n"
          ++"\t    return a+b\n"
          ++"\tshould be equivalent with\n"
          ++"\t  < (x > 0) ? 1+1 : 0+0>") $ do
    it "observation with context 1 should be 3" $
      observe ap_do 1 `shouldBe` 2
    it "observation with context 0 should be 0" $
      observe ap_do 0 `shouldBe` 0

  describe ("Bind: simple >>= (\\v -> Faceted (\\y -> y < 2) (v + 1, v + 3))\n"
          ++"\tshoule be equivalent with\n"
          ++"\t  < (x > 0) ? < (y < 2) ? 1+1, 1+3> : < (y < 2) ? 0+1, 0+3> >") $ do
    it "observation with context 2 should be 4" $
      observe (simple >>= (\v -> Faceted (\y -> y < 2) (v + 1, v + 3))) 2 `shouldBe` 4
    it "observation with context 1 should be 2" $
      observe (simple >>= (\v -> Faceted (\y -> y < 2) (v + 1, v + 3))) 1 `shouldBe` 2
    it "observation with context 0 should be 0" $
      observe (simple >>= (\v -> Faceted (\y -> y < 2) (v + 1, v + 3))) 0 `shouldBe` 1


  describe ("Do Syntax:\n"
          ++"\tdo a <- simple\n"
          ++"\t   b <- simple\n"
          ++"\t   Faceted (\\y -> y < 2) (a*b, a+b)\n"
          ++"\tshoule be equivalent with\n"
          ++"\t  < (x > 0) ? < (y < 2) ? 1*1, 1+1> : < (y < 2) ? 0*0, 0-0> >") $ do
    it "observation with context 2 should be 2" $
      observe monad_do 2 `shouldBe` 2
    it "observation with context 1 should be 1" $
      observe monad_do 1 `shouldBe` 1
    it "observation with context 0 should be 0" $
      observe monad_do 0 `shouldBe` 0
