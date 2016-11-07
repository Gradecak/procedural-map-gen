module Main where

import BSPGen
import Graphics.Gloss


{-Main-}
main :: IO ()
main = do
  let main_room = room (-350,-350) 700 700
  s <- splitContainer main_room 12
  --print $ show s
  let p = drawTree s
  display (InWindow "Map" (1000, 1000) (0, 0)) black $ pictures p

  --render defaultWindow (tree (room (1.5) (1.5) 4 4))
