A Lighted Wreath
================

**Introduction**

This project is to model a wreath with embedded colored lights, to help
determine where to drill holes for the lights to protrude from the back. The wreath
will be cut from plywood, with a symbolic candle and flame rising from the bottom.
The flame will get yellow bulbs, the candle white ones, and the rest of the total of
100 of various colors will be scattered around the wreath. While the candle and flame
will have closely packed geometrically organized bulbs, the wreath is supposed to have
a haphazard look, as if the lights had been tossed by hand. The wreath might get real
or artificial greens added around the lights, but that's beyond the scope of this exercise.

> {-# LANGUAGE NoMonomorphismRestriction #-}
> module Wreath ( litWreath, litWreath'
>               , wreath, wreath', WreathOptions(..)
>               , candleWithFlame, candleWithFlame', CandleWithFlameOptions(..)
>               , wreathLamps', WreathLampOptions(..), wreathLampPoints
>               , candleLamps6', candleLamps9', CandleLampOptions(..)
>               , candleLampsClosePack3x', candleLampsClosePack3xPoints
>               , flameLamps', FlameLampOptions(..)
>               , litLamp', crossMarker', LampOptions(..)
>               , diamond, numDiamondPts
>               , flameShape, flameShapePoints
>               , grid, grid', GridOptions(..))
> where 

> import Diagrams.Prelude
> import Diagrams.Backend.SVG
> import Diagrams.Coordinates
> import Data.Default.Class

**Tuning Parameters**

Some aspects of the model are derived, but there are a number of tuning parameters,
all of which are accessible via some options parameter. The options parameters are
all instances of `Default`, although they don't support the `Lens` syntax (because I don't
know how to do that yet).

Changes to `LampOptions` in any of the higher-level options parameter sets are not
automatically propagated to the others.

> data LampOptions = LampOptions {
>       lampRadius                  :: Double
>     , lampGlowRadiusRatio         :: Double
>     , lampGlowSmoothness          :: Int
>     , lampGlowOpacityRatio        :: Double
>     , lampGlowInitialOpacityRatio :: Double
>     }

> instance Default LampOptions where
>     def = LampOptions {
>       lampRadius                  = 0.03125 -- for C7 lamp
>     , lampGlowRadiusRatio         = 4.0
>     , lampGlowSmoothness          = 6
>     , lampGlowOpacityRatio        = 0.7
>     , lampGlowInitialOpacityRatio = 0.3
>     }

> data WreathLampOptions = WreathLampOptions {
>       wreathLampGetLamps    :: Colour Double -> Diagram B R2
>     , numWreathLamps        :: Int
>     , wreathLampRadii       :: [Double]
>     , wreathLampSalt        :: [Int]
>     , wreathLampColors      :: [Colour Double]
>     , epicycleAmplitudes    :: [Double]
>     , epicycleAngles        :: [Turn]
>     , wreathLampOptions     :: LampOptions
>     }

The available lamp colors are approximated. They are purchased in boxes of 25, 
with 5 each of white, yellow, red, blue, and green. Some whites and yellows are 
allocated to the candle and flame, and the rest go on the wreath. However, over 
time bulbs burn out and are replaced, and new boxes are purchased, so the lamp 
supply is unpredictable. As mentioned in the introduction, the initial supply is 100.

See the comment for the `epicycles` function for an explanation of `epicycleAmplitudes`
and `epicycleAngles`.

> instance Default WreathLampOptions where
>     def = WreathLampOptions {
>       wreathLampGetLamps    = litLamp' lo
>     , numWreathLamps        = 73
>     , wreathLampRadii       = [1.2, 1.5, 1.8]
>     , wreathLampSalt        = [1, 5, 3]
>     , wreathLampColors      = [green, blue, yellow, red, green, yellow, blue]
>     , epicycleAmplitudes    = [0.04,0.07,0.01]
>     , epicycleAngles        = [1/5,3/5,5/5,2/5,4/5]
>     , wreathLampOptions     = lo
>     }
>         where lo = def :: LampOptions

> data CandleLampOptions = CandleLampOptions {
>       candleLampGetLamps     :: Colour Double -> Diagram B R2
>     , candleLampLayout       :: CandleLampOptions -> Diagram B R2
>     , candleLampColor        :: Colour Double
>     , numCandleLamps         :: Int
>     , candleLampSpacingX     :: Double
>     , candleLampSpacingY     :: Double
>     , candleLampVerticalAdj  :: Double
>     , candleLampOptions      :: LampOptions
>     }

> instance Default CandleLampOptions where
>     def = CandleLampOptions {
>       candleLampGetLamps     = litLamp' lo
>     , candleLampLayout       = candleLampsClosePack3x' 6
>     , candleLampColor        = white
>     , numCandleLamps         = 18
>     , candleLampSpacingX     = 0.15
>     , candleLampSpacingY     = 0.15
>     , candleLampVerticalAdj  = -0.93
>     , candleLampOptions      = lo
>     }
>         where lo = def :: LampOptions

> data FlameLampOptions = FlameLampOptions {
>       flameLampGetLamps     :: Colour Double -> Diagram B R2
>     , flameLampLayout       :: Diagram B R2 -> Diagram B R2
>     , flameLampColor        :: Colour Double
>     , numFlameLamps         :: Int
>     , flameLampSpacing      :: Double
>     , flameLampDiamondWidth :: Double
>     , flameLampVerticalAdj  :: Double
>     , flameLampOptions      :: LampOptions
>     }

> instance Default FlameLampOptions where
>     def = FlameLampOptions {
>       flameLampGetLamps     = litLamp' lo
>     , flameLampLayout       = flameShape s n vAdj
>     , flameLampColor        = yellow
>     , numFlameLamps         = numDiamondPts n
>     , flameLampSpacing      = s
>     , flameLampDiamondWidth = n
>     , flameLampVerticalAdj  = vAdj
>     , flameLampOptions      = lo
>     }
>         where lo   = def :: LampOptions
>               s    = 0.15
>               n    = 3
>               vAdj = 0.15

> data WreathOptions = WreathOptions {
>       wreathInnerRadius :: Double
>     , wreathOuterRadius :: Double
>     , wreathFillColor   :: Colour Double
>     , wreathFill        :: Bool
>     , wreathLineWidth   :: Double
>     }

> instance Default WreathOptions where
>     def = WreathOptions { 
>       wreathInnerRadius = 1.0
>     , wreathOuterRadius = 2.0
>     , wreathFillColor   = black
>     , wreathFill        = True
>     , wreathLineWidth   = 0
>     }

> data CandleWithFlameOptions = CandleWithFlameOptions {
>       candleSize                 :: Double
>     , flameSize                  :: Double
>     , flameVerticalAdj           :: Double
>     , candleWithFlameSqueezeX    :: Double
>     , candleWithFlameScale       :: Double
>     , candleWithFlameVerticalAdj :: Double
>     , candleWithFlameFillColor   :: Colour Double
>     , candleWithFlameFill        :: Bool
>     , candleWithFlameLineWidth   :: Double
>     }

The original design had a candle and flame cut from plywood, but
the configuration now calls for a vertical strip across the
diameter, for stability.

> instance Default CandleWithFlameOptions where
>     def = CandleWithFlameOptions {
>       candleSize                 = 1
>     , flameSize                  = 0.0
>     , flameVerticalAdj           = 0.82
>     , candleWithFlameSqueezeX    = 0.3
>     , candleWithFlameScale       = 2.0
>     , candleWithFlameVerticalAdj = 0.0
>     , candleWithFlameFillColor   = black
>     , candleWithFlameFill        = True
>     , candleWithFlameLineWidth   = 0
>     }

The grid is available to put behind a wreath (with `wreathFill` set to `False`)
to create a plan for construction.

> data GridOptions = GridOptions {
>       gridWidth      :: Double
>     , gridHeight     :: Double
>     , gridHLineWidth1 :: Double
>     , gridHLineColor1 :: Colour Double
>     , gridHLineStep1  :: Double
>     , gridHLineWidth2 :: Double
>     , gridHLineColor2 :: Colour Double
>     , gridHLineStep2  :: Double
>     , gridVLineWidth1 :: Double
>     , gridVLineColor1 :: Colour Double
>     , gridVLineStep1  :: Double
>     , gridVLineWidth2 :: Double
>     , gridVLineColor2 :: Colour Double
>     , gridVLineStep2  :: Double
>     }

> instance Default GridOptions where
>     def = GridOptions {
>       gridWidth      = 2
>     , gridHeight     = 2
>     , gridHLineWidth1 = 0.01
>     , gridHLineColor1 = black
>     , gridHLineStep1  = 1
>     , gridHLineWidth2 = 0.01
>     , gridHLineColor2 = lightgray
>     , gridHLineStep2  = (1/12)
>     , gridVLineWidth1 = 0.01
>     , gridVLineColor1 = black
>     , gridVLineStep1  = 1
>     , gridVLineWidth2 = 0.01
>     , gridVLineColor2 = lightgray
>     , gridVLineStep2  = (1/12)
>     }

**Assembling the Wreath**

The lit wreath is an arrangement of lamps on a wreath. Put the wreath on a black 
background to see the effect it would have at night.

**TODO** Deal with the asymmetry in candle and flame lamp function names and abstractions.

> litWreath :: Diagram B R2
> litWreath =  litWreath' def def def def def

> litWreath' :: WreathOptions
>            -> CandleWithFlameOptions
>            -> WreathLampOptions
>            -> CandleLampOptions
>            -> FlameLampOptions
>            -> Diagram B R2
> litWreath' wo cwfo wlo clo flo = lamps `atop` wrth
>     where lamps =  wreathLamps' wlo #  centerXY
>                 <> (candleLampLayout clo) clo
>                 <> flameLamps' flo
>           wrth  =  wreath' wo
>                 <> candleWithFlame' cwfo 
>                 #  scaleX (candleWithFlameSqueezeX cwfo)
>                 #  scale (candleWithFlameScale cwfo)
>                 #  translateY (candleWithFlameVerticalAdj cwfo)

**The Grid**

The grid offers major and minor divisions in horizontal and vertical directions,
with line width and color options.

> grid :: Double -> Double -> Diagram B R2
> grid w h  =  grid' (def { gridWidth = w, gridHeight = h })

> grid' :: GridOptions -> Diagram B R2
> grid' go  =  square 1 # scaleX w # scaleY h # centerXY
>           <> lines (gridHLineWidth1 go) (gridHLineColor1 go) horizontal unitY tickBigYs
>           <> lines (gridVLineWidth1 go) (gridVLineColor1 go) vertical   unitX tickBigXs
>           <> lines (gridHLineWidth2 go) (gridHLineColor2 go) horizontal unitY tickYs
>           <> lines (gridVLineWidth2 go) (gridVLineColor2 go) vertical   unitX tickXs
>     where lines w c f v ss = mconcat (map (lw w . lc c . flip translate f) (map (*^ v) ss))

>           w          = gridWidth go
>           h          = gridHeight go
>           dx1        = gridHLineStep1 go
>           dy1        = gridVLineStep1 go           
>           dx2        = gridHLineStep2 go
>           dy2        = gridVLineStep2 go           
>           horizontal = fromOffsets [w *^ unitX] # centerX
>           vertical   = fromOffsets [h *^ unitY] # centerY

>           tickYs     = [y0, y0 + dy2 .. y1]
>           tickBigYs  = [y0, y0 + dy1 .. y1]
>           y0         = fromIntegral (ceiling (-h/2))
>           y1         = fromIntegral (floor (h/2))

>           tickXs     = [x0, x0 + dx2 .. x1]
>           tickBigXs  = [x0, x0 + dx1 .. x1]
>           x0         = fromIntegral (ceiling (-w/2))
>           x1         = fromIntegral (floor (w/2))

**The Basic Wreath**

A wreath is an annulus. The candle and flame are built up from squares.
With appropriate values of `CandleWithFlameOptions`, the flame can be
hidden and the candle extended into a diameter of the wreath.

> wreath :: Double -> Double -> Diagram B R2
> wreath r1 r2    =  wreath' (def  {wreathOuterRadius = r1, wreathInnerRadius = r2})

> wreath' :: WreathOptions -> Diagram B R2
> wreath' o =  if (wreathFill o) 
>                  then d #  fc (wreathFillColor o)  # fillRule EvenOdd
>                  else d
>     where d = stroke (ring (wreathOuterRadius o) (wreathInnerRadius o))
>             # lw (wreathLineWidth o)

> ring :: Double -> Double -> Path R2
> ring r1 r2 =  circle r1 <> circle r2

> candleWithFlame :: Diagram B R2
> candleWithFlame =  candleWithFlame' def 

> candleWithFlame' :: CandleWithFlameOptions -> Diagram B R2
> candleWithFlame' o =  if (candleWithFlameFill o)
>                           then d #  fc (candleWithFlameFillColor o)
>                           else d
>     where d =  (square (candleSize o)
>             <> square (flameSize o) # rotateBy (1/8) # translateY (flameVerticalAdj o))
>             #  lw (candleWithFlameLineWidth o)

**The Lights**

The wreath lights are at first evenly spaced around concentric circles of the
given radii, then dislodged slightly.

> wreathLamps' :: WreathLampOptions -> Diagram B R2
> wreathLamps' wlo = position (zip (wreathLampPoints wlo) lamps) # centerXY
>     where lamps = map f (cycle cs)
>           f     = wreathLampGetLamps wlo
>           cs    = wreathLampColors wlo

> wreathLampPoints :: WreathLampOptions -> [P2]
> wreathLampPoints wlo = concat [ringPoints n r s wlo | (n,r,s) <- rings]
>     where rs    = wreathLampRadii wlo
>           ss    = wreathLampSalt wlo
>           n     = numWreathLamps wlo
>           db    = tau * (sum rs) / fromIntegral n
>           ns'   = [floor (tau * r / db) | r <- init rs]
>           ns    = ns' ++ [n - sum ns']
>           rings = zip3 ns rs ss

> ringPoints :: Int -> Double -> Int -> WreathLampOptions -> [P2]
> ringPoints n r s wlo = perturb s ps es
>     where ps    = polygon with {_polyType = PolyRegular n r}
>           es    = epicycles amps angs
>           amps  = epicycleAmplitudes wlo
>           angs  = epicycleAngles wlo

Taking inspiration from Ptolemy's epicycles for Mars, we give our lights a
seemingly unpredictable orbit around the candle by dislocating each one by a
small vector. The vector comes from an infinite sequence that cycles through 
simultaneous changes of amplitude and angle. Lastly, we can _salt_ the calculation
by discarding an arbitrary-length prefix of the sequence. "Randomness" without monads!
It's important to keep the lengths of `epicycleAmplitudes` and `epicycleAngles` prime,
so that combined they result in one large cycle. Note that the angles are 
intentionally not sorted.

> perturb :: Int -> [P2] -> [R2] -> [P2]
> perturb n ps es     = [p # translate e | (p,e) <- zip ps (drop n es)]

> dislocate :: (Double, Turn) -> R2
> dislocate (a,t)     = a *^ unitX # rotateBy t

> epicycles :: [Double] -> [Turn] -> [R2]
> epicycles amps angs = map dislocate (zip (cycle amps) (cycle angs))

Simulate a light as a small circle of color, and turn it on by giving it
a soft glow.

> lamp :: Double -> Colour Double -> Diagram B R2
> lamp r c =  circle r # fc c # lw 0

> litLamp' :: LampOptions -> Colour Double -> Diagram B R2
> litLamp' lo c =  lamp (lampRadius lo) c
>               #  withGlow (lampGlowRadiusRatio lo) (lampGlowSmoothness lo) 
>                           (lampGlowOpacityRatio lo) (lampGlowInitialOpacityRatio lo)

The cross marker is an optional "light" that can be placed on a wreath on a grid to
locate a lamp precisely.

> crossMarker' :: LampOptions -> Colour Double -> Diagram B R2
> crossMarker' lo c =  (fromOffsets [ unitX # translate (-0.5 *^ unitX) ] # centerXY
>                   <>  fromOffsets [ unitY # translate (-0.5 *^ unitY) ] # centerXY)
>                   #  fc c # scale (lampRadius lo) # rotateBy (1/8)

Simulate the soft glow of a lit lamp with concentric copies of the
lamp of decreasing opacity. This looks good on-screen, but my experience was
that the real lamp colors were desaturated and the glow effect was different
depending on the background. In fact, the unpainted plywood reflected so much
light that most of the color was washed out. Painting the plywood flat black
helped, at the expense of creating something not very pretty in daylight.

In `withGlow`, _grr_ is the ratio of glow radius to diagram radius (which has only been
tried with circles); _n_ is the number of steps in the geometric interpolation from diagram
radius to glow radius where more steps makes for a smoother appearance; _o_ is the
opacity ratio at each step of interpolation; _i_ is the initial glow opacity ratio,
i.e. applied before interpolation; and _d_ is the diagram to augment with glow.

> withGlow :: Double -> Int -> Double -> Double -> Diagram B R2 -> Diagram B R2
> withGlow grr n o i d =  d <> mconcat (take n (iterate (scale grr' # opacity o) d'))
>     where grr' = grr `to_the` (1 / fromIntegral n)
>           d'   = d # opacity i

> to_the :: Floating a => a -> a -> a
> x `to_the` y = exp (y * (log x))

The candle's lamps are packed into a rectangle with `candleLamps6'` and `candleLamps9'`.
The number of lamps is hardcoded to 6 and 9, respectively. The spacing can be adjusted
via the arguments.

**TODO** This is an obvious place for abstraction.

> candleLamps6'     :: CandleLampOptions -> Diagram B R2
> candleLamps6' clo =  position (zip pts (repeat (getLamp c))) # translateY vAdj
>     where pts     = [ p2 (x, y) | y <- [0, dy, dy*2], x <- [-dx, dx] ]
>           getLamp = candleLampGetLamps clo
>           c       = candleLampColor clo
>           dx      = candleLampSpacingX clo
>           dy      = candleLampSpacingY clo
>           vAdj    = candleLampVerticalAdj clo

> candleLamps9'     :: CandleLampOptions -> Diagram B R2
> candleLamps9' clo =  position (zip pts (repeat (getLamp c))) # translateY vAdj
>     where pts     = [ p2 (x, y) | y <- [0, dy, dy*2], x <- [-dx, 0, dx] ]
>           getLamp = candleLampGetLamps clo
>           c       = candleLampColor clo
>           dx      = candleLampSpacingX clo
>           dy      = candleLampSpacingY clo
>           vAdj    = candleLampVerticalAdj clo

With `candleLampsClosePack3x'`, the lamps are close-packed into three columns,
where _n_ is the number of vertical layers in the stack.

> candleLampsClosePack3x' :: Int -> CandleLampOptions -> Diagram B R2
> candleLampsClosePack3x' n clo =  position (zip ps (repeat getLamp))
>     where getLamp = candleLampGetLamps clo c
>           ps      = candleLampsClosePack3xPoints n clo
>           c       = candleLampColor clo

> candleLampsClosePack3xPoints :: Int -> CandleLampOptions -> [P2]
> candleLampsClosePack3xPoints n clo = ps # translateY vAdj
>     where ps = [ p2 (x, y - dy') | y <- [0, dy .. dy*(fromIntegral n-1)]
>                                  , x <- [-dx, 0, dx]
>                                  , let dy' = if x == 0 then dy / 2 else 0 ]
>           dx   = candleLampSpacingX clo
>           dy   = candleLampSpacingY clo
>           vAdj = candleLampVerticalAdj clo

The flame's lamps are arranged according to a layout function, such
as `diamond` or `flameShape`.

> flameLamps'     :: FlameLampOptions -> Diagram B R2
> flameLamps' flo =  layout (getLamp c)
>     where getLamp = flameLampGetLamps flo
>           layout  = flameLampLayout flo
>           c       = flameLampColor flo

For a diamond shape, _n_ is the number of points across the middle, and _s_ is the distance
between points. The vertical distance between rows is _s_ * (sqrt 3)/2, or _s_ * 0.866,
because the rows are offset from each other so that two horizontal points and the point
above and between them form an equilateral triangle.

**TODO** The diamond shape doesn't recognize the vertical adjustment
**TODO** the diamond points aren't factored out so can't be used by the lamp tour solver

> diamond :: Double -> Double -> Diagram B R2 -> Diagram B R2
> diamond s n d = position (zip ps (repeat d))
>     where ps          = concat $ map (mkPts s n) [0..n-1]
>           mkPts s n 0 =  [p2 (x0 + s * i, 0)  | i <- [0..n-1], let x0 = -s * (n-1) / 2]
>           mkPts s n j =  concat [[p2 (x0 + s * i, y), p2 (x0 + s * i, -y)]  | i <- [0..n-1-j], 
>                              let x0 = -s * (n-1-j) / 2, let y = s * j * 0.866]

> numDiamondPts :: Int -> Int
> numDiamondPts 1 = 1
> numDiamondPts n = numDiamondPts (n-1) + 2 * n - 1

The flame shape is an alteration of the diamond in which the bottommost point is removed,
and a point is added (i.e. the _flicker_) above the topmost and slightly to the right, 
to line up with the point below it. Then the entire collection is rotated so that the 
new topmost point is roughly centered but slightly off. It works well for _n_ = 3 but 
hasn't been tested for any other values.

The arguments are the same, with the addition of _dy_ which is the vertical adjustment
of the entire shape.

**TODO** the angle of rotation should be put into FlameLampOptions

> flameShape :: Double -> Double -> Double -> Diagram B R2 -> Diagram B R2
> flameShape s n dy d = position (zip (flameShapePoints s n dy) (repeat d))

> flameShapePoints :: Double -> Double -> Double -> [P2]
> flameShapePoints s n dy = (topPart ++ bottomPart ++ flicker) 
>                         # rotateBy (1/40) # translateY dy 
>     where
>           mkPts s n 0 =  [p2 (x0 + s * i, 0)  | i <- [0..n-1], let x0 = -s * (n-1) / 2]
>           mkPts s n j =  [p2 (x0 + s * i, y)  | i <- [0..n-1-j], 
>                              let x0 = -s * (n-1-j) / 2, let y = s * j * 0.866]
>           mkPts' s n j = [p2 (x0 + s * i, -y) | i <- [0..n-1-j], 
>                              let x0 = -s * (n-1-j) / 2, let y = s * j * 0.866]
>           topPart    = concat (map (mkPts  s n) [0..n-1])
>           bottomPart = concat (map (mkPts' s n) [1..n-2])
>           flicker    = [p2 (x, y')]
>               where  (x,y) = unp2 (last (init (topPart)))
>                      y' = y + 2 * s * 0.866
