{-# OPTIONS_GHC -w #-}
module Main (main) where

import           Codec.Picture
import           Codec.Picture.Drawing
import           Control.Monad (void)
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Codec.Picture.Types (MutableImage)
import           Data.Foldable

type MImage m px = MutableImage (PrimState m) px

mkYellow :: Double -> PixelRGB8
mkYellow brightness = let rg = round (brightness * 255) in PixelRGB8 rg rg 0

plot
    :: PrimMonad m
    => MImage m PixelRGB8
    -> Int
    -> Int
    -> Double
    -> m ()
plot m x y c = writePixel m x y (mkYellow c)

ipart :: Double -> Int
ipart = truncate

fpart :: Double -> Double
fpart x
    | x > 0 = x - temp
    | otherwise = x - (temp + 1)
    where temp = fromIntegral (ipart x)

rfpart :: Double -> Double
rfpart x = 1 - fpart x

swapIf :: Bool -> (a, a) -> (a, a)
swapIf False c = c
swapIf True (x, y) = (y, x)

newDrawLine
    :: MImage IO PixelRGB8
    -> Int
    -> Int
    -> Int
    -> Int
    -> IO ()
newDrawLine m p1x p1y p2x p2y = do
    let steep = abs (p2y - p1y) > abs (p2x - p1x)
        ((p3x, p4x), (p3y, p4y)) = swapIf steep ((p1x, p2x), (p1y, p2y))
        ((ax, ay), (bx, by)) = swapIf (p3x > p4x) ((p3x, p3y), (p4x, p4y))
        dx = bx - ax
        dy = by - ay
        gradient = if dx == 0 then 1.0 else fromIntegral dy / fromIntegral dx

    -- handle first endpoint
    let xpxl1 = ax -- round (fromIntegral ax)
        yend1 = fromIntegral ay + gradient * fromIntegral (xpxl1 - ax)
        xgap1 = rfpart (fromIntegral ax + 0.5)
    endpoint m steep xpxl1 yend1 xgap1

    -- handle second endpoint
    let xpxl2 = bx -- round (fromIntegral bx)
        yend2 = fromIntegral by + gradient * fromIntegral (xpxl2 - bx)
        xgap2 = fpart (fromIntegral bx + 0.5)
    endpoint m steep xpxl2 yend2 xgap2

    -- main loop
    let intery = yend1 + gradient
    void $ if steep
        then foldlM (\i x -> do
            plot m (ipart i) x (rfpart i)
            plot m (ipart i + 1) x (fpart i)
            pure $ i + gradient) intery [xpxl1 + 1..xpxl2 - 1]
        else foldlM (\i x -> do
            plot m x (ipart i) (rfpart i)
            plot m x (ipart i + 1) (fpart i)
            pure $ i + gradient) intery [xpxl1 + 1..xpxl2 - 1]

endpoint
    :: MImage IO PixelRGB8
    -> Bool
    -> Int
    -> Double
    -> Double
    -> IO ()
endpoint m True xpxl yend xgap = do
    plot m ypxl xpxl (rfpart yend * xgap)
    plot m (ypxl + 1) xpxl (fpart yend * xgap)
    where ypxl = ipart yend
endpoint m False xpxl yend xgap = do
    plot m xpxl ypxl (rfpart yend * xgap)
    plot m xpxl (ypxl + 1) (fpart yend * xgap)
    where ypxl = ipart yend

main :: IO ()
main = do
    let w = 50
        h = 50

    img <- withMutableImage w h (PixelRGB8 0 0 0) $ \m -> do
        newDrawLine m 1 1 49 24

    writePng "/home/rcook/Desktop/test.png" img
