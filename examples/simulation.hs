-- A Lighted Wreath
-- ================
-- 
-- This program draws the wreath with all its default values.

{-# LANGUAGE NoMonomorphismRestriction #-}
import Wreath
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- Run the program with `dist/build/simulation/simulation -o simulation.svg -w 400` 
-- where `-o` sets the output filename, and `-w` sets the diagram width.

main = defaultMain (litWreath # bg black)
