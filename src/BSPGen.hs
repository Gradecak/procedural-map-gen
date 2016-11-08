module BSPGen (Point,Container,Tree,                                  -- datatypes
               container, tree, room,                                 -- constructors
               splitContainer, getLeafs, genPaths, genMap,            -- map gen functions
               drawContainer, drawTree, drawRooms, drawPaths, drawMap -- drawing functions (specific to Graphics.Gloss)
              ) where

import System.Random (randomRIO)
import Control.Monad (mapM)
import Graphics.Gloss (Point, Picture, translate, rectangleWire, rectangleSolid,
                       green, greyN, color, pictures, polygon)


{-  Containers -}
{- Used in defining an area for which a room can be created in-}
data Container = Container Point Float Float

{- Trees -}
{- Basis of the Binary Space Partition Algorithm -}
{- Used to sub divide containers and provide a path between them -}
data Tree a = Nil
            | Leaf (Tree a) a (Tree a)
            deriving Show

{- Path between point a and point b and the width of the path -}
data Path = Path Point Point Float

{- Rooms -}
type Room = Container

--constructors
container :: Point -> Float -> Float -> Container
container = Container

path :: Point -> Point -> Float -> Path
path = Path

tree ::  a -> Tree a
tree a = Leaf Nil a Nil

-- generate a room from a container by preforming some random padding addition and substitution
room :: Container -> IO Room
room (Container (x,y) w h) = do
  xPad <- randomRIO (0, w/3)
  yPad <- randomRIO (0, h/3)
  let nx = x + xPad
      ny = y + yPad
      nw = w - (nx -x)
      nh = h - (ny -y)
  wPad <- randomRIO (0, nw/3)
  hPad <- randomRIO (0, nw/3)
  return $ container (nx,ny) (nw-wPad) (nh-hPad)

-- helper functions
getVal :: Tree a -> a
getVal (Leaf _ v _ ) = v

isNil :: Tree a -> Bool
isNil Nil    = True
isNil Leaf{} = False

-- return a list of containers at the bottom of the tree
getLeafs :: Tree a -> [a]
getLeafs Nil = []
getLeafs (Leaf Nil a Nil) = [a]
getLeafs (Leaf t1 _ t2) = left ++ right
  where left = getLeafs t1
        right = getLeafs t2

-- split a container into a tree of containers
splitContainer :: Container -> Int -> IO (Tree Container)
splitContainer r iter | iter == 0 = return $ tree r
                      | otherwise = do
                          (split1, split2) <- randomSplit r
                          left <- splitContainer split1 (iter -1)
                          right <- splitContainer split2 (iter-1)
                          return $ Leaf left r right

-- randomly split a container horizontaly or vertically
randomSplit :: Container -> IO (Container, Container)
randomSplit r = do
  a <- randomRIO (0,1) :: IO Int
  case a of
    0 -> verticalSplit r
    1 -> horizontalSplit r

verticalSplit :: Container -> IO (Container, Container)
verticalSplit r@(Container (x,y) w h) = do
  newW <- randomRIO (1, w)
  let r1 = container (x,y) newW h
      r2 = container (x+newW,y) (w - newW) h
  checkWRatio r (r1, r2)

horizontalSplit :: Container -> IO (Container, Container)
horizontalSplit r@(Container (x,y) w h) = do
  newH <- randomRIO (1, h)
  let r1 = container (x,y) w newH
      r2 = container (x,y+newH) w (h-newH)
  checkHRatio r (r1, r2)

checkWRatio :: Container -> (Container,Container) -> IO (Container, Container)
checkWRatio r rs@(Container _ w h , Container _ w1 h1) =
  let r1 = w / h
      r2 = w1 / h1
  in if r1 < 0.45 || r2 < 0.45
     then randomSplit r
     else return rs

checkHRatio :: Container -> (Container,Container) -> IO (Container, Container)
checkHRatio r rs@(Container _ w h , Container _ w1 h1) =
  let r1 = h / w
      r2 = h1 / w1
  in if r1 < 0.45 || r2 < 0.45
     then randomSplit r
     else return rs

{- Generator functions for a map-}
-- generate a path between two containers
-- width of the hallways should be teaked according to scale of the map
genPath :: Container -> Container -> Path
genPath (Container (x1,y1) w h) (Container (x2,y2) w1 h1) = path (x1+(w/2), y1+(h/2)) (x2+(w1/2), y2+(h1/2)) 5

--generate a list of paths from a tree
genPaths :: Tree Container ->  [Path]
genPaths (Leaf r _ l )
  | isNil r = []
  | isNil l = []
  | otherwise = [genPath (getVal r) (getVal l)] ++ genPaths r ++ genPaths l

genMap :: Point -> Float -> Float -> Int -> IO (Tree Container, [Room], [Path])
genMap p w h iter = do
  containers <- splitContainer (container p w h) iter
  rooms <- mapM room (getLeafs containers)
  let paths = genPaths containers
  return (containers, rooms, paths)

{- Drawing functions for Graphics.Gloss -}
drawContainer :: Container -> Picture
drawContainer (Container (x,y) w h) = color green $ translate (x+(w/2)) (y+(h/2)) $ rectangleWire w h

drawTree :: Tree Container -> [Picture]
drawTree Nil = []
drawTree (Leaf l x r) = [drawContainer x]++ drawTree l ++ drawTree r

drawRoom :: Room -> Picture
drawRoom (Container (x,y) w h) =  color (greyN 0.7) $ translate (x+(w/2)) (y+(h/2)) $ rectangleSolid w h

drawRooms :: [Room] -> Picture
drawRooms r = pictures $ map drawRoom r

drawPath :: Path -> Picture
drawPath (Path (x1,y1) (x2,y2) w)
  | x1 == x2 = color (greyN 0.7) $ polygon [(x1-w, y1), (x2-w,y2), (x2+w, y2), (x1+w, y1)]
  | otherwise = color (greyN 0.7) $ polygon [(x1,y1-w), (x2, y2-w), (x2, y2+w), (x1, y1+w)]

drawPaths :: [Path] -> Picture
drawPaths p = pictures $ map drawPath p

drawMap :: (Tree Container, [Room], [Path]) -> Picture
drawMap (c, r, p) = pictures [rooms, paths, containers]
  where containers = pictures $ drawTree c
        rooms      = drawRooms r
        paths      = drawPaths p
