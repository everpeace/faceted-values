{-# LANGUAGE DeriveFunctor #-}
----------------------
-- | Faceted values with faceted execution.
--
-- Inspired by Thomas H. Austin and Cormac Flanagan,
-- <http://users.soe.ucsc.edu/~cormac/papers/popl12b.pdf "Multiple Facets for Dynamic Information Flow.">
----------------------
module Data.Faceted (Faceted, (?), (??), (.:), principal, runFaceted, observe) where

import Control.Monad.Free
import Text.Show.Functions

-- | Faceted Value: \< k ? V_H : V_L\>
--
-- Principal is modeled by (ctx -> Bool). True means observer is private one, false means private one.
--
-- Facted values can be declared in very intuitive manner: (\\x -> x > 0) ? 1 .: 0
--
-- Use (??) operator instead of (?) when you declare nested faceted values: (\\x -> x > 0) ?? ((\\y -> y > 1) ? 4 .: 3) .: ((\\z -> z < 2) ? 2 .: 1)
type Faceted ctx a = Free (Facets ctx) a
data Facets ctx a = Facet (ctx -> Bool) a a
                  | Facets (ctx -> Bool) (Facets ctx a) (Facets ctx a)
                    deriving (Show, Functor)

facet :: (ctx -> Bool) -> (a,a) -> Faceted ctx a
facet p (h, l) = Free (Facet p (Pure h) (Pure l))

facets :: (ctx -> Bool) -> (Faceted ctx a, Faceted ctx a) -> Faceted ctx a
facets p (Free h, Free l) = Free (Facets p h l)

infix 1 ??, ?
-- | Constructor for unnested faceted values
-- e.g. (\x -> x > 0) ? 2 .: 1)
(?) :: (ctx -> Bool) -> (a, a) -> Faceted ctx a
(?) = facet

-- | Constructor for nested faceted values
-- e.g. (\\x -> x > 0) ?? ((\\y -> y > 1) ? 4 .: 3) .: ((\\z -> z < 2) ? 2 .: 1)
(??) :: (ctx -> Bool) -> (Faceted ctx a, Faceted ctx a) -> Faceted ctx a
(??) = facets


infix 2 .:
-- | This works as a part of faceted values constructor.
(.:) :: a -> a -> (a,a)
(.:) x y = (x,y)

-- | principal extractor
principal :: Facets ctx val -> (ctx -> Bool)
principal (Facets  p _ _) = p
principal (Facet p _ _) = p

-- internal faceted value executor
run :: Facets ctx a -> ctx -> a
run (Facets p vH vL) ctx = if p ctx then run vH ctx else run vL ctx
run (Facet  p vH vL) ctx = if p ctx then vH else vL

-- | Evaluator for faceted values.
-- Its semantics is faceted execution denoted in the paper above.
--
-- >>> runFaceted ( (const True) ? True .: False) 1
-- True
-- >>> runFaceted ( (const False) ? True .: False) 1
-- False
--
-- nested faceted value.
-- >>> runFaceted ((\x -> x <= 2) ?? ((\x -> x < 2) ? 1 .: 2) .: ((\x -> x < 4 ) ? 3 .: 4)) 2
-- 2
--
-- faceted is applicative.
-- >>> import Control.Applicative
-- >>> runFaceted ( (+) <$> ((\x -> 0 < x && x < 3) ? 1 .: 2) <*> ((\x -> 1 < x && x < 4) ? 4 .: 8)) 3
-- 6
--
-- faceted is monad.
-- >>> runFaceted (((\x -> 0 < x && x < 3) ? 1 .: 2) >>= \v -> ((\x -> 1 < x && x < 4) ? (v+4) .: (v+8))) 3
-- 6
runFaceted :: Faceted ctx a -> ctx -> a
runFaceted (Pure a) ctx = a
runFaceted (Free (Facet  p vH vL)) ctx = if p ctx then runFaceted vH ctx else runFaceted vL ctx
runFaceted (Free (Facets p vH vL)) ctx = if p ctx then runFaceted (run vH ctx) ctx else runFaceted (run vL ctx) ctx

-- | an idiom of runFaceted
observe :: Faceted ctx a -> ctx -> a
observe = runFaceted
