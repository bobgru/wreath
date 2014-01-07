How to String the Wreath Lamps
==============================

**Introduction**

With the wreath lamp layout in hand, we need to install the
string of lamps. That is, in what order should we push the
lamp sockets through the holes in the plywood?

This is an instance of the Traveling Salesman Problem in which the
cities are lamp sockets, with constraints:
* The distance between lamps on the string is 6", according to the
  maker of the string. Assuming less than 6" would give us some slack
  to handle errors in hole position, slight differences in length
  between sockets on the string, and the need to twist the socket to
  get the clip into its hole.
* The male end of the string needs to be in a convenient place, such as
  the very bottom, so that a power cord can be easily connected.
* The string should be extended wherever possible to keep the cord
  flat against the back of the plywood. This is a soft constraint,
  that is, it can be used to improve on a valid walk through the lamps,
  but we can omit it if desired. This translates to a goal of maximizing
  the length of a walk within the hard constraints.

**Strategy**

I'll call the solution a _walk_. It's not a _closed walk_, or _tour_,
because it doesn't return to its starting point.

The wreath will be mounted on a wall, and the exterior electrical outlets
for its power are near the ground, so the second constraint points to
the bottommost hole in the wreath as the ending point.

A reasonable starting point is the tip of the flame, as it's the most
isolated part of the wreath. It isn't strictly necessary, but we'll go
with it.

The problem is exponential in the number of lamps, being an instance of TSP.
Finding the optimal, i.e. longest, walk would require finding all walks, which
is not feasible with 100 lamps. Instead, we will take a greedy approach—using
the longest available move at each step—to solve a specified  number of walks, and 
take the longest. At the same time, we will subdivide the lamps into smaller
walks which connect to each other to form the overall walk.

Note that a greedy approach can lead to dead ends, if there are no valid next
moves but not all lamps have been visited. We will backtrack from failures to
the next available move. Here we take advantage of Haskell's lazy evaluation
by seemingly calculating all walks, but only using (i.e. forcing evaluation of)
a subset.

The subdivision strategy, as with the choice of starting and ending points,
relies on knowledge of the problem. The first subdivision is the flame and candle.
The wreath lamps will be partitioned into quadrants. It would be a simple matter
to generalize the wreath lamp partitioning into an arbitrary number of segments,
and the candle and flame could be similarly broken up, if another wreath were
designed with more lamps.

I will try to factor out the problem-independent parts.

> module Wreath.Walk ( lampWalkMain
>                    , choices
>                    )
> where
> import Diagrams.Backend.SVG.CmdLine
> import Diagrams.Prelude
> import Data.List ((\\), sort)
> import Data.Maybe (fromJust,isNothing)
> import Data.Default.Class
> import Text.Printf
> import System.Exit(exitFailure)
> import Wreath

**Types**

We'll start by defining _walk_ as a list of lamp positions.
A _leg_ is a walk from a starting point to an ending point through a list of
points. An _itinerary_ is a list of legs.

Finding a walk means finding an itinerary and a walk for each leg.

Finding an itinerary means finding a bridge set.

> type Walk      = [P2]
> type Leg       = (P2, P2, [P2])
> type Itinerary = [Leg]
> type Bridge    = (Double, P2, P2)
> type BridgeSet = (Bridge, Bridge, Bridge)

**Itineraries**

Given starting and ending points, the full list of points annotated with quadrant,
and a bridge set, arrange the legs of an itinerary for the entire walk.

> itinerary :: P2 -> P2 -> [(Int, P2)] -> BridgeSet -> Itinerary
> itinerary s e qs (b1, b2, b3) =
>     [ (s, p1, ps1), (p2, p3, ps2), (p4, p5, ps3), (p6, e, ps4) ]
>     where bridgePts b = (s, e) where (_, s, e) = b
>           (p1, p2) = bridgePts b1
>           (p3, p4) = bridgePts b2
>           (p5, p6) = bridgePts b3
>           quadPts s qs = (map snd . filter (\(q,_)->q==q')) qs \\ [s]
>               where q' = quadrant s
>           ps1 = quadPts s  qs
>           ps2 = quadPts p2 qs
>           ps3 = quadPts p4 qs
>           ps4 = quadPts p6 qs

To find the bridges of a bridge set, we need to know the order in which the 
quadrants will be traversed, and all the points annotated with quadrant. We'll 
generate the list of all possible combinations and rely on a lazy consumer.

> bridgeSets :: [Int] -> [(Int, P2)] -> [BridgeSet]
> bridgeSets qo qs = map (\[x,y,z]->(x,y,z)) bs'
>     where bs     = [bridges q1 q2 qs | (q1, q2) <- qpairs]
>           qpairs = zip (init qo) (tail qo)
>           bs'    = choices [head bs, bs !! 1, bs !! 2]

The `bridgeSets` function returns all possible sets of bridge choices, one bridge per
pair of quadrants. The bulk of that work is to enumerate the choices, which is a
generic operation to find choices from the members of a set of sets.

> choices :: [[a]] -> [[a]]
> choices []       = []
> choices [xs]     = [[x] | x <- xs]
> choices (xs:xss) = case xs of
>     []      -> []
>     [x]     -> map (x:) (choices xss)
>     (x:xs') -> map (x:) (choices xss) ++ choices (xs':xss)

Between adjacent quadrants there may be many bridges.
The bridges consist of all pairs of points in adjacent quadrants that satisfy the
hard constraints (encapsulated in the `ok` function). Give them all and let the
consumer be lazy. We include the distance so we can sort, but that's a whim. We
could just as reasonably sort in the reverse order, or not sort at all.

> bridges :: Int -> Int -> [(Int, P2)] -> [Bridge]
> bridges q1 q2 qs = sort [ (distanceSq p1 p2, p1, p2)
>                             | p1 <- (map snd . filter (\(q,p)->q==q1)) qs
>                             , p2 <- (map snd . filter (\(q,p)->q==q2)) qs
>                             , ok p1 p2 ]

**Subdivision into Quadrants**

To subdivide the walk, sort the points into quadrants, determine whether 
we're walking clockwise or counterclockwise, find the bridges and
create the itinerary. There is a different subdivision for each bridge set,
so a leg subdivides into a list of possible itineraries.

> subdivisions :: Leg -> [Itinerary]
> subdivisions (s, e, ps) = map (itinerary s e qs) bss
>     where bss = bridgeSets qo qs'
>           qs  = quadrants ps
>           qs' = quadrants (ps \\ [s, e])
>           qo  = quadrantOrder s e

The quadrant is determined by the signs of a point's _x_ and _y_ coordinates.

> quadrant :: P2 -> Int
> quadrant p = let (x, y) = unp2 p in
>     case (signum x, signum y) of
>         ( 1,  1) -> 1
>         (-1,  1) -> 2
>         (-1, -1) -> 3
>         ( 1, -1) -> 4

> quadrants :: [P2] -> [(Int, P2)]
> quadrants ps = [(quadrant p, p) | p <- ps]

The quadrant order is determined by the quadrants of points _s_ and _e_. Start in
the quadrant of _s_ and move in the opposite direction to the quadrant of _e_.

> quadrantOrder :: P2 -> P2 -> [Int]
> quadrantOrder s e = [q1, q2, q3, q4]
>     where q1 = quadrant s
>           q4 = quadrant e
>           dq = q1 - q4
>           q2 = (q1 + dq) `mod` 4
>           q3 = (q2 + dq) `mod` 4

**Walks**

We have a constraint on the distance between lamps, so let's prepare
a function to check it. We'll only be checking constraints while
evaluating walks for validity, so we can use the squared distance metric.

> ok :: P2 -> P2 -> Bool
> ok a b = distanceSq a b <= maxWalkLeg^2

The maximum distance between lamps should be as low as will allow for walks.
A value of 5.5" is too low—there are no valid walks with that constraint
(for the current default wreath parameters). However, 5.6" does allow walks.
The wreath parameters are expressed in feet, so divide by 12.

> maxWalkLeg = 5.6 / 12 :: Double  -- expressed in feet

At each step, we find all available moves and take the longest.

> nextMoves :: P2 -> [P2] -> [(Double,P2)]
> nextMoves s ps =  (reverse . sort) [(d,p) | p <- ps, ok s p, let d = distanceSq s p]

Enumerate all valid walks for a given leg. We'll delegate to another version
of the function that accumulates subwalks. Allow for failure to find a walk,
indicated by evaluating to `Nothing`.

> allWalks :: Leg -> Maybe [Walk]
> allWalks (s, e, ps) = allWalks' [s] (s, e, ps)

Allow the lazy consumer to specify how many walks to evaluate.

> someWalks :: Int -> Leg -> Maybe [Walk]
> someWalks n (s, e, ps) = case allWalks (s, e, ps) of
>     Nothing -> Nothing
>     Just ws -> Just (take n ws)

In searching for valid walks, we need to deal with the possibility that a partial
walk is a dead end—no remaining points satisfy the constraints. We will implement
a backtracking search, in which we have a partial walk and the valid next moves,
working down the list as long as requested.

Note that the starting point is not included in the set of points to walk through,
but the ending point is.

> allWalks' :: Walk -> Leg -> Maybe [Walk]
> allWalks' _ (_, _, [])  = Nothing
> allWalks' w (s, e, [x]) = if e == x && ok s e then Just [w++[e]] else Nothing
> allWalks' w (s, e, ps)  =
>     if null ps' then Nothing
>                 else Just ((concatMap fromJust . filter (/= Nothing)) ws)
>     where ws  = map (\(d,p) -> (allWalks' (w++[p]) (p, e, ps\\[p]))) ps'
>           ps' = nextMoves s ps

After we get our valid walks, order them best to worst, annotating with
the walk lengths.

> best :: [Walk] -> [(Double, Walk)]
> best = reverse . sort . map (\w -> (walkLength w, w))

> walkLength :: Walk -> Double
> walkLength w = sum (zipWith distance (init w) (tail w))

**Points**

We have established mostly generic plumbing for a solution, specialized only
in the encoding of subdivision into quadrants with three bridges per bridge set.

Now we have to give the specific points, including starting and ending points.
The `Wreath` library exposes functions to get the points for the flame, the candle,
and the wreath. We will invoke these functions in the same way the `simulation`
program does (i.e. with all the default values for parameters), combining the
flame and candle, but subdividing the wreath.

The starting point for the flame and candle is the tip of the flame, the point
with highest _y_-coordinate. The ending point for the flame and candle is the
point with lowest _y_-coordinate.

The starting point for the wreath walk will be the closest point in the wreath to
the end of the candle and flame walk, and the last point in the wreath walk will
be the lowest point on the wreath. At this point we are just creating the overall
leg for the wreath walk.

> candleFlamePts :: Leg
> candleFlamePts = (s, e, ps')
>     where ps  = candlePts ++ flamePts
>           s   = maxY ps
>           e   = minY ps
>           ps' = ps \\ [s]

> flamePts :: [P2]
> flamePts =  flameShapePoints s n vAdj a
>     where s    = flameLampSpacing def
>           n    = flameLampDiamondWidth def
>           vAdj = flameLampVerticalAdj def
>           a    = flameLampRotation def

> candlePts :: [P2]
> candlePts =  candleLampsClosePack3xPoints 6 def

> wreathLampPts :: Leg
> wreathLampPts = (s, e, ps')
>     where ps  = wreathLampPoints def
>           s   = closestTo e0 ps
>           e   = minY ps
>           ps' = ps \\ [s]
>           (_, e0, _) = candleFlamePts

> maxY :: [P2] -> P2
> maxY = p2 . swap . last . sort . map (swap . unp2)
>     where swap~(a,b) = (b,a)

> minY :: [P2] -> P2
> minY = p2 . swap . head . sort . map (swap . unp2)
>     where swap~(a,b) = (b,a)

> closestTo :: P2 -> [P2] -> P2
> closestTo p ps = snd (minimum [(distanceSq p p', p') | p' <- ps])

**Solution**

Here's where we hook up our walk algorithm with our points to solve the problem.
Ask for the best of the first _n_ valid walks through the flame and candle
we can find.

> candleFlameWalk :: Int -> Maybe [(Double, Walk)]
> candleFlameWalk n = case someWalks n candleFlamePts of
>     Nothing  -> Nothing
>     Just ws -> Just (best ws)

Now do the same for the wreath, but subdivide into quadrants, walk those, then
recombine successful subwalks.

> wreathWalk :: Int -> Maybe [(Double, Walk)]
> wreathWalk n = if null ws then Nothing else Just (best ws)
>     where ws = (take n . map concat . filter (not . null))
>                    (map goodSubWalks (subdivisions wreathLampPts))

Given an itinerary, find the first walk for each leg.

> subWalks :: Itinerary -> [Maybe [Walk]]
> subWalks = map (someWalks 1)

Given an itinerary, try to find a walk for each leg. If any leg fails,
report failure for the whole itinerary.

> goodSubWalks :: Itinerary -> [Walk]
> goodSubWalks i = if any null ws then [] else concat ws
>     where fix Nothing   = []
>           fix (Just ws) = ws
>           ws = map fix (subWalks i)

**Formatting Functions**

We may want to see the walk as numbers while working on the algorithm,
so define some formatting functions.

> fmtP2 :: P2 -> String
> fmtP2 p = printf "%0.2f %0.2f" x y
>     where (x,y) = unp2 p

> fmtP2s :: [P2] -> [String]
> fmtP2s = map fmtP2

> fmt :: (Double, Walk) -> (String, [String])
> fmt (d,w) =  (printf "%0.2f" d, fmtP2s w)

> fmtAll :: [(Double, [P2])] -> [(String, [String])]
> fmtAll = map fmt

**Drawing a Graph**

Adapting from the worked example in the Diagrams "Quick start tutorial", we can use the
walk to create a diagram showing it as numbered points with arrows.

> node :: Int -> Diagram B R2
> node n =  text (show n) # fc black # scale s
>        <> circle s      # fc white # named n
>     where s = 0.04

> arrowOpts :: ArrowOpts
> arrowOpts = with & headSize .~ 0.05

The duplicated positioning of nodes is intentional. The `applyAll` causes arrows to be
drawn on top of the numbered nodes, so I draw the nodes first, then draw the nodes with 
the arrows, relying on the rules of composition to put the nodes on top.

> showWalk :: Walk -> Diagram B R2
> showWalk w = ds `atop` ds # applyAll (arrowFuncs (length w))
>     where ds = position (zip w (map node [1..]))
>           arrowFuncs n = [connectOutside' arrowOpts (j::Int) (k::Int)
>                              | (j,k) <- zip [1..n-1] [2..n]]

**Main Program**

Run this with `dist/build/lampWalk/lampWalk -o walk.svg -w 400` to produce a nice graph.
On my laptop it takes about 7 seconds to produce the following as an SVG file, which
I converted to PNG in Inkscape. In the SVG file, the numbers are aligned properly in
the circles.

![Best of 100000 walks](https://github.com/bobgru/diagrams/blob/master/wreath/best-walk.png?raw=true "Best of 100000 walks")

> lampWalkMain :: IO ()
> lampWalkMain = do
>     let w1 = candleFlameWalk 100000
>     let w2 = wreathWalk 100000
>     if isNothing w1 || isNothing w2
>         then do
>             putStrLn "There are no walks"
>             exitFailure 
>         else do 
>             let w1' = (snd . head . fromJust) w1
>             let w2' = (snd . head . fromJust) w2
>             defaultMain (showWalk (w1' ++ w2') # pad 1.1)

