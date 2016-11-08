module Main where

import BSPGen (genMap, drawMap)
import Graphics.Gloss

{-Main-}
main :: IO ()
main = do
  generated_map <- genMap (-350,-350) 700 700 5 -- generate a map 
  display (InWindow "Map" (701, 701) (250, 0)) black $ drawMap generated_map

