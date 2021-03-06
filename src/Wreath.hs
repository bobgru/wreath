-- A Lighted Wreath
-- ================
-- 
-- **Introduction**
-- 
-- This project is to model a wreath with embedded colored lights, to help
-- determine where to drill holes for the lights to protrude from the back. The wreath
-- will be cut from plywood, with a symbolic candle and flame rising from the bottom.
-- The flame will get yellow bulbs, the candle white ones, and the rest of the total of
-- 100 of various colors will be scattered around the wreath. While the candle and flame
-- will have closely packed geometrically organized bulbs, the wreath is supposed to have
-- a haphazard look, as if the lights had been tossed by hand. The wreath might get real
-- or artificial greens added around the lights, but that's beyond the scope of this exercise.

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Wreath ( litWreath, litWreath'
               , wreath, wreath', WreathOptions(..)
               , candleWithFlame, candleWithFlame', CandleWithFlameOptions(..)
               , wreathLamps', WreathLampOptions(..), wreathLampPoints
               , candleLampsClosePack3x', candleLampsClosePack3xPoints
               , CandleLampOptions(..)
               , flameLamps', FlameLampOptions(..)
               , litLamp', crossMarker', LampOptions(..)
               , diamond, diamondPoints, numDiamondPts
               , flameShape, flameShapePoints
               , grid, grid', GridOptions(..))

where 

import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.SVG
import Diagrams.Coordinates
import Data.Monoid ((<>))

-- **Tuning Parameters**
-- 
-- Some aspects of the model are derived, but there are a number of tuning parameters,
-- all of which are accessible via some options parameter. The options parameters are
-- all instances of `Default`, although they don't support the `Lens` syntax (because I don't
-- know how to do that yet).
-- 
-- Changes to `LampOptions` in any of the higher-level options parameter sets are not
-- automatically propagated to the others.

data LampOptions = LampOptions {
      lampRadius                  :: Double
    , lampGlowRadiusRatio         :: Double
    , lampGlowSmoothness          :: Int
    , lampGlowOpacityRatio        :: Double
    , lampGlowInitialOpacityRatio :: Double
    }

instance Default LampOptions where
    def = LampOptions {
      lampRadius                  = 0.03125 -- for C7 lamp
    , lampGlowRadiusRatio         = 4.0
    , lampGlowSmoothness          = 6
    , lampGlowOpacityRatio        = 0.7
    , lampGlowInitialOpacityRatio = 0.3
    }

data WreathLampOptions = WreathLampOptions {
      wreathLampGetLamps    :: Colour Double -> Diagram B
    , numWreathLamps        :: Int
    , wreathLampRadii       :: [Double]
    , wreathLampSalt        :: [Int]
    , wreathLampColors      :: [Colour Double]
    , epicycleAmplitudes    :: [Double]
    , epicycleAngles        :: [Double]
    , wreathLampOptions     :: LampOptions
    }

-- The available lamp colors are approximated. They are purchased in boxes of 25, 
-- with 5 each of white, yellow, red, blue, and green. Some whites and yellows are 
-- allocated to the candle and flame, and the rest go on the wreath. However, over 
-- time bulbs burn out and are replaced, and new boxes are purchased, so the lamp 
-- supply is unpredictable. As mentioned in the introduction, the initial supply is 100.
-- 
-- See the comment for the `epicycles` function for an explanation of `epicycleAmplitudes`
-- and `epicycleAngles`.

instance Default WreathLampOptions where
    def = WreathLampOptions {
      wreathLampGetLamps    = litLamp' lo
    , numWreathLamps        = 73
    , wreathLampRadii       = [1.2, 1.5, 1.8]
    , wreathLampSalt        = [1, 5, 3]
    , wreathLampColors      = [green, blue, yellow, red, green, yellow, blue]
    , epicycleAmplitudes    = [0.04,0.07,0.01]
    , epicycleAngles        = [1/5,3/5,5/5,2/5,4/5]
    , wreathLampOptions     = lo
    }
        where lo = def :: LampOptions

data CandleLampOptions = CandleLampOptions {
      candleLampGetLamps     :: Colour Double -> Diagram B
    , candleLampLayout       :: CandleLampOptions -> Diagram B
    , candleLampColor        :: Colour Double
    , numCandleLamps         :: Int
    , candleLampSpacingX     :: Double
    , candleLampSpacingY     :: Double
    , candleLampVerticalAdj  :: Double
    , candleLampOptions      :: LampOptions
    }

instance Default CandleLampOptions where
    def = CandleLampOptions {
      candleLampGetLamps     = litLamp' lo
    , candleLampLayout       = candleLampsClosePack3x' 6
    , candleLampColor        = white
    , numCandleLamps         = 18
    , candleLampSpacingX     = 0.15
    , candleLampSpacingY     = 0.15
    , candleLampVerticalAdj  = -0.93
    , candleLampOptions      = lo
    }
        where lo = def :: LampOptions

data FlameLampOptions = FlameLampOptions {
      flameLampGetLamps     :: Colour Double -> Diagram B
    , flameLampLayout       :: Diagram B -> Diagram B
    , flameLampColor        :: Colour Double
    , numFlameLamps         :: Int
    , flameLampSpacing      :: Double
    , flameLampDiamondWidth :: Double
    , flameLampVerticalAdj  :: Double
    , flameLampHorizAdj     :: Double
    , flameLampRotation     :: Double
    , flameLampOptions      :: LampOptions
    }

instance Default FlameLampOptions where
    def = FlameLampOptions {
      flameLampGetLamps     = litLamp' lo
    , flameLampLayout       = flameShape s n vAdj hAdj a
    , flameLampColor        = yellow
    , numFlameLamps         = numDiamondPts n
    , flameLampSpacing      = s
    , flameLampDiamondWidth = n
    , flameLampVerticalAdj  = vAdj
    , flameLampHorizAdj     = hAdj
    , flameLampRotation     = a
    , flameLampOptions      = lo
    }
        where lo   = def :: LampOptions
              s    = 0.15
              n    = 3
              vAdj = 0.15
              hAdj = 0
              a    = 1/40

data WreathOptions = WreathOptions {
      wreathInnerRadius :: Double
    , wreathOuterRadius :: Double
    , wreathFillColor   :: Colour Double
    , wreathFill        :: Bool
    , wreathLineWidth   :: Double
    }

instance Default WreathOptions where
    def = WreathOptions { 
      wreathInnerRadius = 1.0
    , wreathOuterRadius = 2.0
    , wreathFillColor   = black
    , wreathFill        = True
    , wreathLineWidth   = 0
    }

data CandleWithFlameOptions = CandleWithFlameOptions {
      candleSize                 :: Double
    , flameSize                  :: Double
    , flameVerticalAdj           :: Double
    , candleWithFlameSqueezeX    :: Double
    , candleWithFlameScale       :: Double
    , candleWithFlameVerticalAdj :: Double
    , candleWithFlameFillColor   :: Colour Double
    , candleWithFlameFill        :: Bool
    , candleWithFlameLineWidth   :: Double
    }

-- The original design had a candle and flame cut from plywood, but
-- the configuration now calls for a vertical strip across the
-- diameter, for stability.

instance Default CandleWithFlameOptions where
    def = CandleWithFlameOptions {
      candleSize                 = 1
    , flameSize                  = 0.0
    , flameVerticalAdj           = 0.82
    , candleWithFlameSqueezeX    = 0.3
    , candleWithFlameScale       = 2.0
    , candleWithFlameVerticalAdj = 0.0
    , candleWithFlameFillColor   = black
    , candleWithFlameFill        = True
    , candleWithFlameLineWidth   = 0
    }

-- The grid is available to put behind a wreath (with `wreathFill` set to `False`)
-- to create a plan for construction.

data GridOptions = GridOptions {
      gridWidth      :: Double
    , gridHeight     :: Double
    , gridHLineWidth1 :: Double
    , gridHLineColor1 :: Colour Double
    , gridHLineStep1  :: Double
    , gridHLineWidth2 :: Double
    , gridHLineColor2 :: Colour Double
    , gridHLineStep2  :: Double
    , gridVLineWidth1 :: Double
    , gridVLineColor1 :: Colour Double
    , gridVLineStep1  :: Double
    , gridVLineWidth2 :: Double
    , gridVLineColor2 :: Colour Double
    , gridVLineStep2  :: Double
    }

instance Default GridOptions where
    def = GridOptions {
      gridWidth      = 2
    , gridHeight     = 2
    , gridHLineWidth1 = 0.01
    , gridHLineColor1 = black
    , gridHLineStep1  = 1
    , gridHLineWidth2 = 0.01
    , gridHLineColor2 = lightgray
    , gridHLineStep2  = 1/12
    , gridVLineWidth1 = 0.01
    , gridVLineColor1 = black
    , gridVLineStep1  = 1
    , gridVLineWidth2 = 0.01
    , gridVLineColor2 = lightgray
    , gridVLineStep2  = 1/12
    }

-- **Assembling the Wreath**
-- 
-- The lit wreath is an arrangement of lamps on a wreath. Put the wreath on a black 
-- background to see the effect it would have at night.
-- 
-- **TODO** Deal with the asymmetry in candle and flame lamp function names and abstractions.

litWreath :: Diagram B
litWreath =  litWreath' def def def def def

litWreath' :: WreathOptions
           -> CandleWithFlameOptions
           -> WreathLampOptions
           -> CandleLampOptions
           -> FlameLampOptions
           -> Diagram B
litWreath' wo cwfo wlo clo flo = lamps `atop` wrth
    where lamps =  wreathLamps' wlo #  centerXY
                <> candleLampLayout clo clo
                <> flameLamps' flo
          wrth  =  wreath' wo
                <> candleWithFlame' cwfo 
                #  scaleX (candleWithFlameSqueezeX cwfo)
                #  scale (candleWithFlameScale cwfo)
                #  translateY (candleWithFlameVerticalAdj cwfo)

-- **The Grid**
-- 
-- The grid offers major and minor divisions in horizontal and vertical directions,
-- with line width and color options.

grid :: Double -> Double -> Diagram B
grid w h  =  grid' (def { gridWidth = w, gridHeight = h })

grid' :: GridOptions -> Diagram B
grid' go  =  square 1 # scaleX w # scaleY h # centerXY
          <> lines (gridHLineWidth1 go) (gridHLineColor1 go) horizontal unitY tickBigYs
          <> lines (gridVLineWidth1 go) (gridVLineColor1 go) vertical   unitX tickBigXs
          <> lines (gridHLineWidth2 go) (gridHLineColor2 go) horizontal unitY tickYs
          <> lines (gridVLineWidth2 go) (gridVLineColor2 go) vertical   unitX tickXs
    where lines w c f v ss = mconcat (map ((lwG w . lc c . flip translate f) . (*^ v)) ss)

          w          = gridWidth go
          h          = gridHeight go
          dx1        = gridHLineStep1 go
          dy1        = gridVLineStep1 go           
          dx2        = gridHLineStep2 go
          dy2        = gridVLineStep2 go           
          horizontal = fromOffsets [w *^ unitX] # centerX
          vertical   = fromOffsets [h *^ unitY] # centerY

          tickYs     = [y0, y0 + dy2 .. y1]
          tickBigYs  = [y0, y0 + dy1 .. y1]
          y0         = fromIntegral (ceiling (-h/2))
          y1         = fromIntegral (floor (h/2))

          tickXs     = [x0, x0 + dx2 .. x1]
          tickBigXs  = [x0, x0 + dx1 .. x1]
          x0         = fromIntegral (ceiling (-w/2))
          x1         = fromIntegral (floor (w/2))

-- **The Basic Wreath**
-- 
-- A wreath is an annulus. The candle and flame are built up from squares.
-- With appropriate values of `CandleWithFlameOptions`, the flame can be
-- hidden and the candle extended into a diameter of the wreath.

wreath :: Double -> Double -> Diagram B
wreath r1 r2    =  wreath' (def  {wreathOuterRadius = r1, wreathInnerRadius = r2})

wreath' :: WreathOptions -> Diagram B
wreath' o =  if wreathFill o
                 then d #  fc (wreathFillColor o)  # fillRule EvenOdd
                 else d
    where d = stroke (ring (wreathOuterRadius o) (wreathInnerRadius o))
            # lwG (wreathLineWidth o)

ring :: Double -> Double -> Path V2 Double
ring r1 r2 =  circle r1 <> circle r2

candleWithFlame :: Diagram B
candleWithFlame =  candleWithFlame' def 

candleWithFlame' :: CandleWithFlameOptions -> Diagram B
candleWithFlame' o =  if candleWithFlameFill o
                          then d #  fc (candleWithFlameFillColor o)
                          else d
    where d =  (square (candleSize o)
            <> square (flameSize o) # rotateBy (1/8) # translateY (flameVerticalAdj o))
            #  lwG (candleWithFlameLineWidth o)

-- **The Lights**
-- 
-- The wreath lights are at first evenly spaced around concentric circles of the
-- given radii, then dislodged slightly.

wreathLamps' :: WreathLampOptions -> Diagram B
wreathLamps' wlo = position (zip (wreathLampPoints wlo) lamps) # centerXY
    where lamps = map f (cycle cs)
          f     = wreathLampGetLamps wlo
          cs    = wreathLampColors wlo

wreathLampPoints :: WreathLampOptions -> [P2 Double]
wreathLampPoints wlo = concat [ringPoints n r s wlo | (n,r,s) <- rings]
    where rs    = wreathLampRadii wlo
          ss    = wreathLampSalt wlo
          n     = numWreathLamps wlo
          db    = tau * sum rs / fromIntegral n
          ns'   = [floor (tau * r / db) | r <- init rs]
          ns    = ns' ++ [n - sum ns']
          rings = zip3 ns rs ss

ringPoints :: Int -> Double -> Int -> WreathLampOptions -> [P2 Double]
ringPoints n r s wlo = perturb s ps es
    where ps    = polygon with {_polyType = PolyRegular n r}
          es    = epicycles amps angs
          amps  = epicycleAmplitudes wlo
          angs  = epicycleAngles wlo

-- Taking inspiration from Ptolemy's epicycles for Mars, we give our lights a
-- seemingly unpredictable orbit around the candle by dislocating each one by a
-- small vector. The vector comes from an infinite sequence that cycles through 
-- simultaneous changes of amplitude and angle. Lastly, we can _salt_ the calculation
-- by discarding an arbitrary-length prefix of the sequence.
-- 
-- It's important to keep the lengths of `epicycleAmplitudes` and `epicycleAngles` prime,
-- so that combined they result in one large cycle. Note that the angles are 
-- intentionally not sorted.
-- 
-- Maybe see what real randomness does sometime?

perturb :: Int -> [P2 Double] -> [V2 Double] -> [P2 Double]
perturb n ps es     = [p # translate e | (p,e) <- zip ps (drop n es)]

dislocate :: (Double, Double) -> V2 Double
dislocate (a,t)     = a *^ unitX # rotateBy t

epicycles :: [Double] -> [Double] -> [V2 Double]
epicycles amps angs = zipWith (curry dislocate) (cycle amps) (cycle angs)

-- Simulate a light as a small circle of color, and turn it on by giving it
-- a soft glow.

lamp :: Double -> Colour Double -> Diagram B
lamp r c =  circle r # fc c # lw 0

litLamp' :: LampOptions -> Colour Double -> Diagram B
litLamp' lo c =  lamp (lampRadius lo) c
              #  withGlow (lampGlowRadiusRatio lo) (lampGlowSmoothness lo) 
                          (lampGlowOpacityRatio lo) (lampGlowInitialOpacityRatio lo)

-- The cross marker is an optional "light" that can be placed on a wreath on a grid to
-- locate a lamp precisely.

crossMarker' :: LampOptions -> Colour Double -> Diagram B
crossMarker' lo c =  (fromOffsets [ unitX # translate (-0.5 *^ unitX) ] # centerXY
                  <>  fromOffsets [ unitY # translate (-0.5 *^ unitY) ] # centerXY)
                  #  fc c # scale (lampRadius lo) # rotateBy (1/8)

-- Simulate the soft glow of a lit lamp with concentric copies of the
-- lamp of decreasing opacity. This looks good on-screen, but my experience was
-- that the real lamp colors were desaturated and the glow effect was different
-- depending on the background. In fact, the unpainted plywood reflected so much
-- light that most of the color was washed out. Painting the plywood flat black
-- helped, at the expense of creating something not very pretty in daylight.
-- 
-- In `withGlow`, _grr_ is the ratio of glow radius to diagram radius (which has only been
-- tried with circles); _n_ is the number of steps in the geometric interpolation from diagram
-- radius to glow radius where more steps makes for a smoother appearance; _o_ is the
-- opacity ratio at each step of interpolation; _i_ is the initial glow opacity ratio,
-- i.e. applied before interpolation; and _d_ is the diagram to augment with glow.

withGlow :: Double -> Int -> Double -> Double -> Diagram B -> Diagram B
withGlow grr n o i d =  d <> mconcat (take n (iterate (scale grr' # opacity o) d'))
    where grr' = grr `toThe` (1 / fromIntegral n)
          d'   = d # opacity i

toThe :: Floating a => a -> a -> a
x `toThe` y = exp (y * log x)

-- With `candleLampsClosePack3x'`, the lamps are close-packed into three columns,
-- where _n_ is the number of vertical layers in the stack.

candleLampsClosePack3x' :: Int -> CandleLampOptions -> Diagram B
candleLampsClosePack3x' n clo =  position (zip ps (repeat getLamp))
    where getLamp = candleLampGetLamps clo c
          ps      = candleLampsClosePack3xPoints n clo
          c       = candleLampColor clo

candleLampsClosePack3xPoints :: Int -> CandleLampOptions -> [P2 Double]
candleLampsClosePack3xPoints n clo = ps # translateY vAdj
    where ps = [ p2 (x, y - dy') | y <- [0, dy .. dy*(fromIntegral n-1)]
                                 , x <- [-dx, 0, dx]
                                 , let dy' = if x == 0 then dy / 2 else 0 ]
          dx   = candleLampSpacingX clo
          dy   = candleLampSpacingY clo
          vAdj = candleLampVerticalAdj clo

-- The flame's lamps are arranged according to a layout function, such
-- as `diamond` or `flameShape`.

flameLamps'     :: FlameLampOptions -> Diagram B
flameLamps' flo =  layout (getLamp c)
    where getLamp = flameLampGetLamps flo
          layout  = flameLampLayout flo
          c       = flameLampColor flo

-- For a diamond shape, _n_ is the number of points across the middle, and _s_ is the distance
-- between points. The vertical distance between rows is _s_ * (sqrt 3)/2, or _s_ * 0.866,
-- because the rows are offset from each other so that two horizontal points and the point
-- above and between them form an equilateral triangle. The arguments _dy_, _dx_, and _a_
-- are, respectively, the vertical adjustment, horizontal adjustment, and rotation of
-- the entire collection of points.

diamond :: Double -> Double -> Double -> Double -> Double -> Diagram B -> Diagram B
diamond s n dy dx a d = position (zip ps (repeat d))
    where ps = diamondPoints s n dy dx a

diamondPoints :: Double -> Double -> Double -> Double -> Double -> [P2 Double]
diamondPoints s n dy dx a = ps # rotateBy a # translateY dy # translateX dx
    where ps          = concatMap (mkPts s n) [0..n-1]
          mkPts s n 0 =  [p2 (x0 + s * i, 0)  | i <- [0..n-1], let x0 = -s * (n-1) / 2]
          mkPts s n j =  concat [[p2 (x0 + s * i, y), p2 (x0 + s * i, -y)]  | i <- [0..n-1-j], 
                             let x0 = -s * (n-1-j) / 2, let y = s * j * 0.866]

numDiamondPts :: Int -> Int
numDiamondPts 1 = 1
numDiamondPts n = numDiamondPts (n-1) + 2 * n - 1

-- The flame shape is an alteration of the diamond in which the bottommost point is removed,
-- and a point is added (i.e. the _flicker_) above the topmost and slightly to the right, 
-- to line up with the point below it. Then the entire collection is rotated so that the 
-- new topmost point is roughly centered but slightly off. It works well for _n_ = 3 but 
-- hasn't been tested for any other values.
-- 
-- The arguments are the same as for the diamond shape.

flameShape :: Double -> Double -> Double -> Double -> Double -> Diagram B -> Diagram B
flameShape s n dy dx a d = position (zip (flameShapePoints s n dy dx a) (repeat d))

flameShapePoints :: Double -> Double -> Double -> Double -> Double -> [P2 Double]
flameShapePoints s n dy dx a = (topPart ++ bottomPart ++ flicker) 
                        # rotateBy a # translateY dy # translateX dx
    where
          mkPts s n 0 =  [p2 (x0 + s * i, 0)  | i <- [0..n-1], let x0 = -s * (n-1) / 2]
          mkPts s n j =  [p2 (x0 + s * i, y)  | i <- [0..n-1-j], 
                             let x0 = -s * (n-1-j) / 2, let y = s * j * 0.866]
          mkPts' s n j = [p2 (x0 + s * i, -y) | i <- [0..n-1-j], 
                             let x0 = -s * (n-1-j) / 2, let y = s * j * 0.866]
          topPart    = concatMap (mkPts  s n) [0..n-1]
          bottomPart = concatMap (mkPts' s n) [1..n-2]
          flicker    = [p2 (x, y')]
              where  (x,y) = unp2 (last (init topPart))
                     y' = y + 2 * s * 0.866
