----------------------
-- | Faceted Values with Faceted Execution.
--
-- Inspired by Thomas H. Austin and Cormac Flanagan,
-- <http://users.soe.ucsc.edu/~cormac/papers/popl12b.pdf "Multiple Facets for Dynamic Information Flow.">
----------------------
module Data.Faceted (Faceted (Faceted), principal, runFaceted, observe) where

import Control.Applicative
import Text.Show.Functions

-- | Faceted Value: \< k ? V_H : V_L\>
--
-- Principal is modeled by (ctx -> Bool). True means observer is private one, false means private one.
--
-- Note that faceted value can be nested: \< k1 ? \< k2 ? V_H : V_L\> : \< k3 ? W_H : W_L\> \>
data Faceted ctx val = CFaceted (ctx -> Bool) (Faceted ctx val) (Faceted ctx val)
                     | Faceted  (ctx -> Bool) val val
                       deriving (Show)

-- |
-- principal extractor
principal :: Faceted ctx val -> (ctx -> Bool)
principal (Faceted  p _ _) = p
principal (CFaceted p _ _) = p

-- | Evaluator for faceted values.
-- Its semantics is faceted execution denoted in the paper above.
--
-- >>> runFaceted (Faceted (const True) True False) 1
-- True
-- >>> runFaceted (Faceted (const False) True False) 0
-- False
runFaceted :: Faceted ctx val -> ctx -> val
runFaceted (CFaceted p fh fl) ctx = if p ctx then (runFaceted fh ctx)
                                              else (runFaceted fl ctx)
runFaceted (Faceted  p vh vl) ctx = if p ctx then vh else vl

-- | an idiom of runFaceted
observe    :: Faceted ctx val -> ctx -> val
observe = runFaceted

-- | Faceted is a functor in terms of value
--
-- >>> observe (fmap (+3) (Faceted (\x -> x > 0) 1 0)) 0
-- 3
-- >>> observe (fmap (+3) (Faceted (\x -> x > 0) 1 0)) 1
-- 4
instance Functor (Faceted ctx) where
    fmap f (CFaceted p fh fl) = CFaceted p (fmap f fh) (fmap f fl)
    fmap f (Faceted  p vh vl) = Faceted  p (f vh)      (f vl)


-- | Faceted is an applicative functor in iterms of value.
--
-- >>> observe ((+) <$> Faceted (\x -> x > 0) 1 0 <*> Faceted (\x -> x > 0) 1 0) 0
-- 0
-- >>> observe ((+) <$> Faceted (\x -> x > 0) 1 0 <*> Faceted (\x -> x > 0) 1 0) 1
-- 2
instance Applicative (Faceted ctx) where
    pure val = Faceted (const True) val val
    Faceted  pf fh fl <*> a = CFaceted pf (fmap fh a) (fmap fl a)
    CFaceted pf fh fl <*> a = CFaceted pf (fh <*> a)  (fl <*> a)


-- | Facted is an Monad in terms of value.
--
-- >>> observe (Faceted (\x -> x > 0) 1 0 >>= \a -> Faceted (\y -> y > 1) (a+2) (a+1) ) 0
-- 1
-- >>> observe (Faceted (\x -> x > 0) 1 0 >>= \a -> Faceted (\y -> y > 1) (a+2) (a+1) ) 1
-- 2
-- >>> observe (Faceted (\x -> x > 0) 1 0 >>= \a -> Faceted (\y -> y > 1) (a+2) (a+1) ) 2
-- 3
instance Monad (Faceted ctx) where
    return = pure
    Faceted  pa ah al >>= f = CFaceted pa (f ah)    (f al)
    CFaceted pa ah al >>= f = CFaceted pa (ah >>=f) (al >>= f)
