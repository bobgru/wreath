A Plan for Building a Christmas Wreath
======================================

This program overrides the default values of all wreath options is such a way as
to produce a precise engineering drawing (except for the lack of labels). The
scale is 1 square per inch.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Wreath
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import Data.Default.Class

Run the program with `dist/build/wreathPlan/wreathPlan -o wreath.svg -w 400` 
where `-o` sets the output filename, and `-w` sets the diagram width.

> main = defaultMain plan

> plan = litWreath' wo cwfo wlo clo flo `atop` grid' go

The lamp options are shared among various other option sets. We make the
"lamp radius" larger so the cross marker is more easily visible.

> lo :: LampOptions
> lo = def { lampRadius = 0.1 }

We want the wreath to not be filled so the grid shows through. The same goes
for the candle and flame. We also need to set the line width to make it visible,
as by default it is 0.

> wo :: WreathOptions
> wo = def { 
>       wreathFill      = False 
>     , wreathLineWidth = 0.03 
>     }

> cwfo :: CandleWithFlameOptions
> cwfo = def { 
>       candleWithFlameFill      = False 
>     , candleWithFlameLineWidth = 0.03
>     }

The lamps are drawn as black crosses.

> wlo :: WreathLampOptions
> wlo = def { 
>       wreathLampGetLamps    = crossMarker' lo
>     , wreathLampColors      = repeat black
>     , wreathLampOptions     = lo
> }

> clo :: CandleLampOptions
> clo = def {
>       candleLampGetLamps    = crossMarker' lo
>     , candleLampColor       = black
>     , candleLampOptions     = lo
> }

> flo :: FlameLampOptions
> flo = def {
>       flameLampGetLamps    = crossMarker' lo
>     , flameLampColor       = black
>     , flameLampOptions     = lo
> }

The grid encompasses the entire wreath.

> go :: GridOptions
> go = def { gridWidth = w, gridHeight = w }
>     where w = 2 * wreathOuterRadius wo
