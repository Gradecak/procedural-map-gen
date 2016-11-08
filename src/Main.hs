module Main where

import BSPGen
import Graphics.Gloss
import Control.Monad (mapM)


{-Main-}
main :: IO ()
main = do
  s <- splitContainer (container (-350,-350) 700 700) 10 -- generate a tree of containters
  r <-  mapM room (getLeafs s)
  let containers = pictures $ drawTree s
      rooms      = drawRooms r
      paths      = drawPaths $ genPaths s
  display (InWindow "Map" (701, 701) (250, 0)) black $ pictures [containers, paths, rooms]

