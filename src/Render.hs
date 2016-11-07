module Render(Window,defaultWindow,samples,render) where

import BSPGen


{- Drawing functions -}

--move cursor to poisiion (x,y)
goto :: Int -> Int -> IO ()
goto x y    = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"
-- clear terminal screen
cls :: IO ()
cls = putStr "\ESC[2J"

--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered, 
--             and the size of the output device to render into
data Window = Window Point Point (Int,Int)

makeWindow :: Point -> Point -> (Int,Int) -> Window
makeWindow p0 p1 size = Window p0 p1 size

-- Default window renders a small region around the origin into
-- a 50x50 pixel output window
defaultWindow :: Window
defaultWindow = makeWindow (point (-1.5) (-1.5)) (point 1.5 1.5) (50,50)


-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / (fromIntegral $ n-1) .. ]

-- Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ]

-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]

render :: Window -> Tree Room -> IO ()
render win tree = sequence_ $ map pix locations
  where
    pix (p, (x, y)) | p `ptInTree` tree = goto x y >> putChar '*'
                    | otherwise         = print p >> return ()
    locations :: [ (Point, (Int, Int))]
    locations = concat $ zipWith zip (pixels win) (coords win)
-- -- render a drawing into a window
-- render :: Window -> Drawing -> IO ()
-- render win sh =  sequence_ $ map pix locations
--   where
--     pix (p,(x,y)) | p `inside` sh = goto x y  >> color (colourAt p sh) "*"
--                   | otherwise     = return ()
--     -- locations is a list of abstract coords ("pixels") and
--     -- corresponding screen coords
--     locations :: [ (Point, (Int,Int) ) ]
--     locations = concat $ zipWith zip (pixels win) (coords win)
