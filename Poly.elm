module Poly (inPolygon) where

import Graphics.Collage (Shape)

type Point = (Float, Float)
data Line = Sloped Float Float | Vert Float

polygonSides : Shape -> [(Point, Point)]
polygonSides poly = zip poly <| (tail poly) ++ [head poly]

intersects : Point -> Line -> Bool
intersects (px, py) line =
    case line of
      Vert xint  -> px <= xint
      Sloped m b -> if | m < 0     -> py <= m * px + b
                       | otherwise -> py >= m * px + b

onLine : Point -> Line -> Bool
onLine (px, py) line =
    case line of
      Vert xint  -> px == xint
      Sloped m b -> py == m * px + b

carrier : (Point, Point) -> Line
carrier ((ax, ay), (bx, by)) =
    let slope = (ay - by) / (ax - bx)
        yint  = ay - slope * ax
    in if | ax == bx  -> Vert ax
          | otherwise -> Sloped slope yint

between : Float -> Float -> Float -> Bool
between x a b =
    if | a > b     -> b <= x && x <= a
       | otherwise -> a <= x && x <= b

odd : Int -> Bool
odd n = (mod n 2) == 1

fp p n side rest =
    let line = carrier side
        (px, py) = p
        ((ax, ay), (bx, by)) = side
        rayIntersects =
            intersects p line &&
            (py /= ay || by < py) &&
            (py /= by || ay < py)
        onSegment = if | ay == by  -> between px ax bx
                       | otherwise -> onLine p line
        far = not <| between py ay by
    in if | far           -> f p n rest
          | onSegment     -> True
          | rayIntersects -> f p (n + 1) rest
          | otherwise     -> f p n rest

f p n sides =
    case sides of
      []             -> odd n
      (side :: rest) -> fp p n side rest

inPolygon : Point -> Shape -> Bool
inPolygon p = f p 0 . polygonSides
