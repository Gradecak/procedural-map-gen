module Main where

import BSPGen
import Graphics.Gloss
import Control.Monad (mapM)


{-Main-}
main :: IO ()
main = do
  let main_container = container (-350,-350) 700 700
  s <- splitContainer main_container 4
  --print $ show s
  let containers = pictures $ drawTree s
  rooms <-  mapM room (getLeafs s)
  let roomPic = pictures $ drawRooms  rooms
  display (InWindow "Map" (1000, 1000) (0, 0)) black $ pictures [containers, roomPic]

  --render defaultWindow (tree (room (1.5) (1.5) 4 4))
