{-# LANGUAGE DeriveFunctor #-}
----------------------
-- | Faceted values with faceted execution
--
-- Inspired by Thomas H. Austin and Cormac Flanagan,
-- <http://users.soe.ucsc.edu/~cormac/papers/popl12b.pdf "Multiple Facets for Dynamic Information Flow.">
--
-- A faceted value \< k ? V_H : V_L\> is a triple consisting of a principal @k@ and two values @V_H@ and @V_L@.
-- Intuitively, this faceted value appears as @V_H@ to private observers that can view @k@’s private data,
-- and as @V_L@ to other public observers. We refer to @V_H@ and @V_L@ as private and public facets, respectively.
--
-- This module supports intuitive syntax suger by (?) and (.:) operators.
-- Use (??) operator instead of (?) when you declare nested faceted values.
-- In this module, principal is modeled by @(ctx -> Bool)@.
-- Returning @True@ means @ctx@ is a private observer, @False@ means it is a public observer.
--
-- @
-- (\x -> x > 0) ? 1 .: 0
-- (\x -> x > 0) ?? ((\y -> y > 1) ? 4 .: 3) .: ((\z -> z < 2) ? 2 .: 1)
-- @
--
-- To observe faceted value, use 'observe' or 'runFaceted' function.
-- When observing faceted values, returning True means that the observer is private one, False means public one.
--
-- @
-- observe ((\x -> x ==\"Me\") ? 1 .: 0) \"Me\"
-- 1
-- observe ((\x -> x =="Me") ? 1 .: 0) \"SomeOne\"
-- 0
-- @
----------------------
module Data.Faceted (Faceted, (?), (??), (.:), constF, principal, runFaceted, observe) where

import Control.Monad.Free
import Text.Show.Functions

-- | Type for Faceted Value: \< k ? V_H : V_L\>
--
-- Please Note that this type utilizes `Control.Monad.Free`.
type Faceted ctx a = Free (Facets ctx) a
data Facets ctx a = Facet (ctx -> Bool) a a
                  | Facets (ctx -> Bool) (Facets ctx a) (Facets ctx a)
                    deriving (Show, Functor)

facet :: (ctx -> Bool) -> (a,a) -> Faceted ctx a
facet p (h, l) = Free (Facet p (Pure h) (Pure l))

-- | @ (constF a) @ is equivalent with @ (const True) ? a .: a @
--
-- This would be useful when you declare like below:
--
-- @
-- ( \"Me\" ?? (constF myLocation) .: \"CoWorker\" ? \"Office\" .: \"In the City\")
-- @
constF :: a -> Faceted ctx a
constF a = facet (const True) (a, a)

facets :: (ctx -> Bool) -> (Faceted ctx a, Faceted ctx a) -> Faceted ctx a
facets p (Free h, Free l) = Free (Facets p h l)
facets p (Pure h, Free l) = facets p (constF h, Free l)
facets p (Free h, Pure l) = facets p (Free h, constF l)
facets p (Pure h, Pure l) = facet p (h, l)

infix 1 ??, ?
-- | Constructor for unnested faceted values
--
-- @
-- (\x -> x > 0) ? 2 .: 1
-- @
--
-- The value in (?) operator will be treated as values in this faceted value.
-- This means that @Faceted ctx (Faceted ctx b)@ can be possible.
-- Note that @Faceted ctx (Faceted ctx b)@ is essentially different from nested faceted values.
(?) :: (ctx -> Bool) -> (a, a) -> Faceted ctx a
(?) = facet

-- | Constructor for nested faceted values
--
-- @
-- (\x -> x > 0) ?? ((\y -> y > 1) ? 4 .: 3) .: ((\z -> z < 2) ? 2 .: 1)
-- @
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

-- >>> runFaceted ( (const True) ? True .: False) 1
-- True
--
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
-- | Evaluator for faceted values.  Its semantics is faceted execution denoted in the paper above.
--
-- @
-- observe ((\x -> x ==\"Me\") ? 1 .: 0) \"Me\"
-- 1
-- observe ((\x -> x =="Me") ? 1 .: 0) \"SomeOne\"
-- 0
-- @
runFaceted :: Faceted ctx a -> ctx -> a
runFaceted (Pure a) ctx = a
runFaceted (Free (Facet  p vH vL)) ctx = if p ctx then runFaceted vH ctx else runFaceted vL ctx
runFaceted (Free (Facets p vH vL)) ctx = if p ctx then runFaceted (run vH ctx) ctx else runFaceted (run vL ctx) ctx

-- | an idiom of runFaceted
observe :: Faceted ctx a -> ctx -> a
observe = runFaceted
