This module is a test driver for `cabal test`.

We use the `quickCheckAll` function, which is written in Template Haskell
so that it can generate code after scanning this module for properties.

> {-# LANGUAGE TemplateHaskell #-}

The module name must be `Main` for `cabal build` to produce an executable,
which we named as a test suite in our `.cabal` file.

> module Main(main) where
> import WalkTests as LW

> import Control.Monad(unless)
> import System.Exit(exitFailure)
> import Test.QuickCheck.All(quickCheckAll)

> main = do
>     allPass <- $quickCheckAll
>     unless allPass exitFailure

As I had all the properties in a different module not named
`Main`, with copious documentation, I decided to redirect to them from here.

> prop_choicesWithNil                      = LW.prop_choicesWithNil
> prop_choicesLengthIsLengthProduct        = LW.prop_choicesLengthIsLengthProduct
> prop_choicesHeadCountIsTailLengthProduct = LW.prop_choicesHeadCountIsTailLengthProduct
> prop_choicesSingletons                   = LW.prop_choicesSingletons

While working out this system it was convenient to have a property
guaranteed to fail. I'll leave it here but commented out.

    prop_alwaysFail = False
