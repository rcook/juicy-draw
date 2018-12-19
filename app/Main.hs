module Main (main) where

import           Codec.Picture
import           Codec.Picture.Drawing

main :: IO ()
main = do
    let w = 480
        h = 360

    img <- withMutableImage w h (PixelRGB8 150 0 0) $ \m -> do
        -- A green diagonal line
        drawLine m 0 0 (w - 1) (h - 1) (PixelRGB8 0 255 0)

        -- A blue square at a 45-degree angle
        drawPolygon m [(50, 50), (75, 75), (100, 50), (75, 25), (50, 50)] (PixelRGB8 0 0 255)

        -- An orange bounding rectangle
        drawRectangle m 0 0 (w - 1) (h - 1) (PixelRGB8 255 150 0)

        -- A mangenta filled rectangle
        fillRectangle m 200 30 250 130 (PixelRGB8 255 0 255)

        -- A dark green filled triangle
        fillTriangle m 50 200 250 300 70 350 (PixelRGB8 0 150 50)

        -- A blue pentagon
        drawPolygon m
            [ (340, 80)
            , (245, 149)
            , (281, 261)
            , (399, 261)
            , (435, 149)
            , (340, 80)
            ]
            (PixelRGB8 0 0 255)

    writePng "example.png" img
