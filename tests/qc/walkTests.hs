-- Testing Wreath.Walk Library Functions
-- ----------------------------------
-- 
{-# LANGUAGE TemplateHaskell #-}

module Main where
import Wreath.Walk
import Test.QuickCheck
import Control.Monad(unless)
import System.Exit(exitFailure)
import Test.QuickCheck.All(quickCheckAll)

-- **Property-based Tests**
-- 
-- * If `choices` is given a list containing an empty list, it returns an empty list.
-- * The length of `choices xss` is the product of the lengths of the members of _xss_.
-- * The head of the head of `choices xss` is the head of _n_ elements, where
--   _n_ is the product of the lengths of the members of `tail xss`.
-- * If _xss_ contains a single member _xs_, then `choices xss` returns a list of
--   singleton lists, one for each member of _xs_.
-- 
-- **Generating Random Data for `choices`**
-- 
-- The `choices` function is polymorphic over list type, so we can
-- use `Int` for simplicity.
-- 
-- First, create a list of between _l_ and _h_ elements. We can allow
-- empty members by passing 0 for _l_.

intList :: Int -> Int -> Gen [Int]
intList l h = do
    m <- choose (l, h)
    s <- vector m
    return s

-- Now create _m_ lists of lists between _l_ and _h_ elements. We can create a
-- singleton list of lists by passing 1 for _m_.

intListList :: Int -> Int -> Int -> Gen [[Int]]
intListList m l h = mapM (\i -> intList l h) [1..m]

-- **The `choices` Tests**
-- 
-- * If `choices` is given a list containing an empty list, it returns an empty list.
-- 
-- This is equivalent to `elem [] xss` implies `choices xss == []`, or
-- `(not . elem) [] xss || choices xss == []`. We'll do the latter, keeping the
-- lists to 5 _x_ 5 or smaller.

data Int1To5 = Int1To5 Int deriving (Show, Eq)

instance Arbitrary Int1To5 where
  arbitrary = Int1To5 <$> choose (1, 5)

prop_choicesWithNil :: Int1To5 -> Property
prop_choicesWithNil (Int1To5 n) = do
    forAll (intListList 5 0 n)  $ \xss -> 
        (not ([] `elem` xss)) || null (choices xss)

-- * The length of `choices xss` is the product of the lengths of the members of _xss_.

prop_choicesLengthIsLengthProduct :: Int1To5 -> Property
prop_choicesLengthIsLengthProduct (Int1To5 n) = do
    forAll (intListList 5 0 n)  $ \xss ->
        length (choices xss) == product (map length xss)

-- * The head of the head of `choices xss` is the head of _n_ elements, where
--   _n_ is the product of the lengths of the members of `tail xss`.
-- 
-- There are several points to note:
-- 
-- * If the tail of _xss_ is empty, then the product of the lengths of its 
--   members is the identity for multiplication, i.e. 1, so the property still holds.
-- * If _xss_ contains an empty member, the head count is 0, so we omit that case.
-- * If `head (head xss)` is not unique within the union of the members of _xss_
--   then the property does not hold, so we fix _xss_ to avoid that condition.

prop_choicesHeadCountIsTailLengthProduct :: Int1To5 -> Property
prop_choicesHeadCountIsTailLengthProduct (Int1To5 n) = do
    forAll (intListList 5 1 n)  $ \xss ->
        let cs  = choices (fix xss)
            h   = (head . head) cs
        in
        length (filter ((h ==) . head) cs) == product (map length (tail xss))
    where fix zss@(xs:xss) = (1 + maximum (concat zss) : tail xs) : xss

-- * If _xss_ contains a single member _xs_, then `choices xss` returns a list of
--   singleton lists, one for each member of _xs_.
-- 
-- We will generate only singleton lists by passing 1 as the first argument of
-- `intListList`.

prop_choicesSingletons :: Int1To5 -> Property
prop_choicesSingletons (Int1To5 n) = do
    forAll (intListList 1 1 n)  $ \xss ->
        let cs = choices xss in
        (length cs == length (head xss)) && all (== 1) (map length cs)

-- This main function will scan the source code for function names beginning with "prop_"
-- and invoke all of them.

return []
runTests = $quickCheckAll

main = do
    allPass <- runTests
    unless allPass exitFailure
