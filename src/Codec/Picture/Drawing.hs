{-|
Module      : Codec.Picture.Draw
Description : Functions for drawing and filling lines, rectangles and polygons directly onto a mutable image
Copyright   : (C) Richard Cook, 2018
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable

Functions for drawing and filling lines, rectangles and polygons directly onto a JuicyPixels mutable image
-}

module Codec.Picture.Drawing
    ( drawLine
    , drawPolygon
    , drawRectangle
    , fillPolygon
    , fillRectangle
    , fillTriangle
    , withDefaultMutableImage
    , withMutableImage
    ) where

import           Codec.Picture.Geometry (Point2D)
import           Codec.Picture.Types
                    ( Image
                    , MutableImage(..)
                    , Pixel
                    , createMutableImage
                    , newMutableImage
                    , unsafeFreezeImage
                    , writePixel
                    )
import           Control.Monad (when)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Foldable (for_)

-- | Create an image given a function to apply to a default empty mutable image
withDefaultMutableImage
    :: (Pixel px, PrimMonad m)
    => Int                                      -- ^ image width
    -> Int                                      -- ^ image height
    -> (MutableImage (PrimState m) px -> m ())  -- ^ function to apply to mutable image
    -> m (Image px)                             -- ^ immutable image result
withDefaultMutableImage w h f = do
    m <- newMutableImage w h
    f m
    unsafeFreezeImage m

-- | Create an image given a function to apply to an empty mutable image
withMutableImage
    :: (Pixel px, PrimMonad m)
    => Int                                      -- ^ image width
    -> Int                                      -- ^ image height
    -> px                                       -- ^ background colour
    -> (MutableImage (PrimState m) px -> m ())  -- ^ function to apply to mutable image
    -> m (Image px)                             -- ^ action
withMutableImage w h px f = do
    m <- createMutableImage w h px
    f m
    unsafeFreezeImage m

-- | Draw a line in the specified colour
drawLine
    :: (Pixel px, PrimMonad m)
    => MutableImage (PrimState m) px    -- ^ mutable image
    -> Int                              -- ^ x-coordinate of starting point
    -> Int                              -- ^ y-coordinate of starting point
    -> Int                              -- ^ x-coordinate of end point
    -> Int                              -- ^ y-coordinate of end point
    -> px                               -- ^ colour
    -> m ()                             -- ^ action
drawLine m x1 y1 x2 y2 colour =
    let dx = fromIntegral (x2 - x1) :: Double
        dy = fromIntegral (y2 - y1) :: Double
    in
        if abs dx > abs dy
            then
                for_ [min x1 x2..max x1 x2] $ \x ->
                    let y = y1 + truncate (dy * fromIntegral (x - x1) / dx)
                    in writePixel m x y colour
            else
                for_ [min y1 y2..max y1 y2] $ \y ->
                    let x = x1 + truncate (dx * fromIntegral (y - y1) / dy)
                    in writePixel m x y colour

-- | Draw a polygon in the specified colour
drawPolygon
    :: (Pixel px, PrimMonad m)
    => MutableImage (PrimState m) px    -- ^ mutable image
    -> [Point2D]                        -- ^ sequence of vertices
    -> px                               -- ^ colour
    -> m ()                             -- ^ action
drawPolygon _ [] _ = pure ()
drawPolygon _ [_] _ = pure ()
drawPolygon m ((x1, y1) : xs@((x2, y2) : _)) colour = do
    drawLine m x1 y1 x2 y2 colour
    drawPolygon m xs colour

-- | Draw a rectangle in the specified colour
drawRectangle
    :: (Pixel px, PrimMonad m)
    => MutableImage (PrimState m) px    -- ^ mutable image
    -> Int                              -- ^ x-coordinate of top-left corner
    -> Int                              -- ^ y-coordinate of top-left corner
    -> Int                              -- ^ x-coordinate of bottom-right corner
    -> Int                              -- ^ y-coordinate of bottom-right corner
    -> px                               -- ^ colour
    -> m ()                             -- ^ action
drawRectangle m x1 y1 x2 y2 = drawPolygon m [(x1, y1), (x2, y1), (x2, y2), (x1, y2), (x1, y1)]

-- | Fill a rectangle with the specified colour
fillRectangle
    :: (Pixel px, PrimMonad m)
    => MutableImage (PrimState m) px    -- ^ mutable image
    -> Int                              -- ^ x-coordinate of top-left corner
    -> Int                              -- ^ y-coordinate of top-left corner
    -> Int                              -- ^ x-coordinate of bottom-right corner
    -> Int                              -- ^ y-coordinate of bottom-right corner
    -> px                               -- ^ colour
    -> m ()                             -- ^ action
fillRectangle m x0 y0 x1 y1 px =
    for_ [(x, y) | x <- [x0..x1], y <- [y0..y1]] $ \(x, y) -> writePixel m x y px

-- | Fill a triangle with the specified colour
fillTriangle
    :: (Pixel px, PrimMonad m)
    => MutableImage (PrimState m) px    -- ^ mutable image
    -> Int                              -- ^ x-coordinate of first vertex
    -> Int                              -- ^ y-coordinate of first vertex
    -> Int                              -- ^ x-coordinate of second vertex
    -> Int                              -- ^ y-coordinate of second vertex
    -> Int                              -- ^ x-coordinate of third vertex
    -> Int                              -- ^ y-coordinate of third vertex
    -> px                               -- ^ colour
    -> m ()                             -- ^ action
fillTriangle m@(MutableImage w h _) v1x v1y v2x v2y v3x v3y px =
    let (minX, maxX) = minMax3 v1x v2x v3x
        (minY, maxY) = minMax3 v1y v2y v3y
        minX' = max minX 0
        minY' = max minY 0
        maxX' = min maxX (w - 1)
        maxY' = min maxY (h - 1)
    in
        for_ [(x, y) | x <- [minX'..maxX'], y <- [minY'..maxY']] $ \(x, y) -> do
            let w0 = orient2D v2x v2y v3x v3y x y
                w1 = orient2D v3x v3y v1x v1y x y
                w2 = orient2D v1x v1y v2x v2y x y
            when (w0 >= 0 && w1 >= 0 && w2 >= 0) $ writePixel m x y px

-- | Fill a polygon as a series of triangles with the specified colour
fillPolygon
    :: (Pixel px, PrimMonad m)
    => MutableImage (PrimState m) px    -- ^ mutable image
    -> [Point2D]                        -- ^ sequence of vertices
    -> px                               -- ^ colour
    -> m ()                             -- ^ action
fillPolygon m ((x1, y1) : vs) px =
    let temp = zip vs (drop 1 vs)
    in for_ temp $ \((x2, y2), (x3, y3)) ->
        fillTriangle m x1 y1 x2 y2 x3 y3 px
fillPolygon _ _ _ = pure ()

orient2D :: Int -> Int -> Int -> Int -> Int -> Int -> Int
orient2D ax ay bx by cx cy = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)

min3 :: Int -> Int -> Int -> Int
min3 a b c
    | a < b = min a c
    | otherwise = min b c

max3 :: Int -> Int -> Int -> Int
max3 a b c
    | a > b = max a c
    | otherwise = max b c

minMax3 :: Int -> Int -> Int -> Point2D
minMax3 a b c = (min3 a b c, max3 a b c)
