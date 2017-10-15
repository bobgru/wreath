-- A Wreath Comparison
-- ===================
-- 
-- This program draws 15 wreaths in a 3 x 5 grid, changing the "salt" parameter of
-- the middle ring of lights on the wreath, so as to visually compare them.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
import Wreath
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG.CmdLine
import Data.Monoid ((<>))

-- Run the program with `dist/build/comparison/comparison -o comparison.svg -w 400` 
-- where `-o` sets the output filename, and `-w` sets the diagram width.

main = defaultMain wreathArray

wreathArray = wreaths # gridLayout 5 bgElement
    where wreaths   = [litWreath' def def (def {wreathLampSalt=[1,n,3]}) def def | n <- [1..15]]
          bgElement = square 1 # lw 0 # fc black # centerXY # pad 1.1

-- Arrange a list of diagrams in a grid. The prototype background element _e_ is scaled
-- to fit the largest diagram.

gridLayout n e ds = grid # centerXY
    where
        grid    = foldr1 (===) rows
        rows    = map (foldr1 (|||)) boxes
        boxes   = chunk n ds''
        ds''    = zipWith (<>) ds' (repeat box)
        ds'     = map centerXY ds
        box     = e # scale boxSize
        boxSize = boxAll ds'
        boxAll  = maximum . map box1
        box1 d  = map (`diameter` d) [unitX, unitY] # maximum

chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)
