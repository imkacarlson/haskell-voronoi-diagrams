module Voronoi where

import RenderBMP
import System.Random
import Data.Bits

---------------

type Space  = (Double,Double)
type Color  = Int
type Pict   = Space -> Color
type Figure = Space -> Maybe Color
[black, white, red, orange, yellow, green, blue, purple, salmon] = 
	[0x000000, 0xffffff, 0xff0000, 0xff9900, 0xffff00, 0x00ff00, 0x0000ff, 0x6600cc, 0xFA8072] :: [Int]

---------------

blank = \(x,y) -> Nothing
when b y = if b then Just y else Nothing
f `orr` b = \x -> case f x of Nothing -> b   ; Just y -> y
f `on`  g = \x -> case f x of Nothing -> g x ; Just y -> Just y
layer figs = foldl on blank figs

---------------

range n = [-m..m] where m = (n-1) / 2
space n m = map (\y -> (map (\x -> (x,y)) (range n))) (reverse (range m))
render fig w h = map (map (scale mag mag (fig `orr` white))) (space w h)
try p = test (round x) (round y) (concat (render p x y))
        where size = base*mag
              x = 2*size-1
              y =   size-1
base = 50.0
mag  =  8.0		-- use (30,3) for Hugs, (50,8) for GHCi (on my Mac)

---------------

dist x y = sqrt (x^2 + y^2)
shiftf n m f = \(x,y) -> f (x-n, y-m)
scale n m f = \(x,y) -> f (x/n, y/m)
skew  n m f = \(x,y) -> f (x-(n*y/m), y)
swap (x,y) = (y,x)
diag f = f . swap
flipv f = \(x,y) -> f (x,-y)
fliph f = \(x,y) -> f (-x,y)

---------------

within x n = -m<=x && x<=m where m = (n-1)/2
rect w h c = \(x,y) -> when (x `within` w && y `within` h) c
circ r c   = \(x,y) -> when (dist x y < r) c
square r c = rect (2*r) (2*r) c
radial  c = \(x,y) -> when (p x y) c
            where p x y = if y==0 then True else even (round (x / y))
stripe k c = skew k k (\(x,y) -> when (round x `mod` round k == 0) c)
checker c = \(x,y) -> when (even (round x + round y)) c

---------------
-- Fritz's Examples
squirc n = circ n yellow `on` square n blue

tree   = shiftf 0 3 (circ 9 green) `on` shiftf 0 (-8) (rect 5 11 black)
target = layer (zipWith circ [1,3..13] (cycle [red,white]))
wagon  = layer [shiftf 0 5 (rect 25 5 red), shiftf (-6) 0 wheel, shiftf 6 0 wheel]
         where wheel = circ 4 black
mickey = layer [circ 9 black, shiftf 8 8 ear, shiftf (-8) 8 ear]
         where ear = circ 5 black
dizzy  = skew 2 3 (mickey `on` radial orange `on` checker yellow)
scene  = tree `on` sun `on` field
         where sun   = shiftf 17 10   (circ 5 yellow)
               field = shiftf 0 (-13) (\(x,y) -> when (y `within` 3) green)
scene2 = shiftf (-18) (-10) (scale 0.75 0.75 wagon) `on` scene
galaxy = skew 3 3 (target `on` scale 2 2 (checker green))
stripy = scale 2 1 (stripe 4 blue)
messy  = skew 2 3 (layer [scale (-2) 2 (stripe 5 purple), target, stripy])

simple = (circ 5 red `on` shiftf 4 0 (rect 10 20 blue)) `on` radial yellow

-- Voronoi Diagrams

-- Colors
shadesOfColor c = [c] ++ shadesOfColor ((shift (((shift c (-8)) ) - 5) 8))

-- Need this to be able to pattern match and get the center of a circle
data Circle center radius color = Circle center radius color deriving Show

drawDarkCircles (Circle (x,y) radius color) = (shiftf x y (circ radius black))
drawCircles circs = map drawDarkCircles circs

repDist (x,y) (Circle (x_1,y_1) radius color) = ((dist' (x,y) (x_1, y_1)), color)
dist' (x_1,y_1) (x_2,y_2) = (sqrt((x_2 - x_1)^2 + (y_2-y_1)^2))

voronoiBackground circs = \(x,y) -> Just (snd (minimum (map (repDist (x,y)) circs)))

-- try (voronoi <n = number of points>)
voronoi n = layer (drawCircles circs) `on` voronoiBackground circs
    where xCord = take n (randomRs (-base::Double, base) (mkStdGen 109))
          yCord = take n (randomRs (-(base/2)::Double, (base/2)) (mkStdGen 99))
          centPoints = zip xCord yCord
          colors = take n (randomRs (1::Int, 16777215) (mkStdGen 986))
          centPointsAndColors = zip centPoints colors
          circs = [Circle cent 0.5 color | (cent, color) <- centPointsAndColors]

-- Checkered Voronoi Diagram
checkerVoronoiBackground circs = \(x,y) -> when (even (round x + round y)) ((snd (minimum (map (repDist (x,y)) circs))))

-- try (checkerVoronoi <n = number of points>)
checkerVoronoi n = layer (drawCircles circs) `on` checkerVoronoiBackground circs
    where xCord = take n (randomRs (-base::Double, base) (mkStdGen 109))
          yCord = take n (randomRs (-(base/2)::Double, (base/2)) (mkStdGen 99))
          centPoints = zip xCord yCord
          colors = take n (randomRs (1::Int, 16777215) (mkStdGen 986))
          centPointsAndColors = zip centPoints colors
          circs = [Circle cent 0.5 color | (cent, color) <- centPointsAndColors]

-- Striped Voronoi Diagram
stripedVoronoiBackground circs = skew 3 3 (\(x,y) -> when (round x `mod` round 3 == 0) (snd (minimum (map (repDist (x,y)) circs))))

-- try (stripeVoronoi <n = number of points>)
stripeVoronoi n = layer (drawCircles circs) `on` stripedVoronoiBackground circs
    where xCord = take n (randomRs (-base::Double, base) (mkStdGen 109))
          yCord = take n (randomRs (-(base/2)::Double, (base/2)) (mkStdGen 99))
          centPoints = zip xCord yCord
          colors = take n (randomRs (1::Int, 16777215) (mkStdGen 986))
          centPointsAndColors = zip centPoints colors
          circs = [Circle cent 0.5 color | (cent, color) <- centPointsAndColors]

-- Shaded Voronoi Diagram

-- try (shadedVoronoi <n = number of points> <color>)
shadedVoronoi n color = layer (drawCircles circs) `on` voronoiBackground circs
    where xCord = take n (randomRs (-base::Double, base) (mkStdGen 109))
          yCord = take n (randomRs (-(base/2)::Double, (base/2)) (mkStdGen 99))
          centPoints = zip xCord yCord
          colors = take n (shadesOfColor (color))
          centPointsAndColors = zip centPoints colors
          circs = [Circle cent 0.5 color | (cent, color) <- centPointsAndColors]

-- Voronoi Circle

-- try (vcirc <r = radius>)
vcirc r  = \(x,y) -> when (dist x y < r) ((snd (minimum (map (repDist (x,y)) circs))))
    where xCord = take 20 (randomRs (-r::Double, r) (mkStdGen 109))
          yCord = take 20 (randomRs (-r::Double, r) (mkStdGen 99))
          centPoints = zip xCord yCord
          --colors = take 20 (shadesOfGreen (green))
          colors = take 20 (randomRs (1::Int, 16777215) (mkStdGen 986))
          centPointsAndColors = zip centPoints colors
          circs = [Circle cent 0.5 color | (cent, color) <- centPointsAndColors]

-- Voronoi Trunk (like a tree trunk)

-- try (vtrunk <w = width, h = height>)
vtrunk w h  = \(x,y) -> when (x `within` w && y `within` h) ((snd (minimum (map (repDist (x,y)) circs))))
    where xCord = take 20 (randomRs (-w::Double, w) (mkStdGen 109))
          yCord = take 20 (randomRs (-h::Double, h) (mkStdGen 99))
          centPoints = zip xCord yCord
          colors = take 20 (shadesOfColor (13789470))
          centPointsAndColors = zip centPoints colors
          circs = [Circle cent 0.5 color | (cent, color) <- centPointsAndColors]

-- Voronoi Tree

-- try vtree
vtree = (vcirc 9) `on` shiftf 0 (-8) (vtrunk 5 22)