{-|
Module      : Codec.Picture.Geometry
Description : Essential geometry for 2D shapes
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

Essential geometric functions to generate 2D shapes and paths
-}

module Codec.Picture.Geometry
    ( FPoint2D
    , Point2D
    , centroid
    , clockwise
    , closed
    ) where

import           Data.Foldable (foldl')
import           Data.List (sortBy)
import           Numeric.Extras (RealExtras, fmod)

-- | 2D coordinate
type Point2D = (Int, Int)

-- | 2D coordinate
type FPoint2D = (Double, Double)

-- | Compute the centroid of a polygon
centroid
    :: [Point2D]        -- ^ sequence of vertices
    -> Maybe FPoint2D   -- ^ centroid
centroid [] = Nothing
centroid ps =
    let (totalX, totalY, n') = foldl' (\(accX, accY, n) (x, y) -> (accX + x, accY + y, n + 1)) (0, 0, 0 :: Int) ps
        n'' = fromIntegral n'
    in Just (fromIntegral totalX / n'', fromIntegral totalY / n'')

-- | Closed sequence of vertices
closed
    :: [Point2D]    -- ^ sequence of vertices
    -> [Point2D]    -- ^ sequence of vertices
closed [] = []
closed ps@(p : _ ) = ps ++ [p]

-- | Sequence of vertices in clockwise order relative to its centroid
clockwise
    :: [Point2D]    -- ^ sequence of vertices
    -> [Point2D]    -- ^ sequence of vertices
clockwise ps =
    case centroid ps of
        Just (cx, cy) -> sortBy (\(ax, ay) (bx, by) ->
            let a1 = angle (fromIntegral ax) (fromIntegral ay) cx cy
                a2 = angle (fromIntegral bx) (fromIntegral by) cx cy
            in compare a2 a1) ps
        Nothing -> []

angle :: RealExtras a => a -> a -> a -> a -> a
angle x y cx cy = (degrees (atan2 (x - cx) (y - cy)) + 360) `fmod` 360

degrees :: Floating a => a -> a
degrees rad = rad * pi / 180
