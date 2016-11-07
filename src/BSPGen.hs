module BSPGen (Point,Container,Tree,
               container, tree, room,
               splitContainer, drawContainer, drawTree, drawRooms, getLeafs) where

import System.Random (randomRIO)
import Graphics.Gloss (Point, Picture, translate, rectangleWire, rectangleSolid, green, greyN, color, pictures)

data Container = Container Point Float Float
  deriving Show

container :: Point -> Float -> Float -> Container
container = Container

drawContainer :: Container -> Picture
drawContainer (Container (x,y) w h) = color green $ translate (x+(w/2)) (y+(h/2)) $ rectangleWire w h

data Tree a = Nil
            | Leaf (Tree a) a (Tree a)
            deriving Show

tree ::  a -> Tree a
tree a = Leaf Nil a Nil

drawTree :: Tree Container -> [Picture]
drawTree Nil = []
drawTree (Leaf l x r) = [drawContainer x]++ drawTree l ++ drawTree r

getLeafs :: Tree a -> [a]
getLeafs Nil = []
getLeafs (Leaf Nil a Nil) = [a]
getLeafs (Leaf t1 _ t2) = left ++ right
  where left = getLeafs t1
        right = getLeafs t2

splitContainer :: Container -> Int -> IO (Tree Container)
splitContainer r iter | iter == 0 = return $ tree r
                 | otherwise = do
                     (split1, split2) <- randomSplit r
                     left <- splitContainer split1 (iter -1)
                     right <- splitContainer split2 (iter-1)
                     return $ Leaf left r right

randomSplit :: Container -> IO (Container, Container)
randomSplit r = do
  a <- randomRIO (0,1) :: IO Int
  case a of
    0 -> verticalSplit r
    1 -> horizontalSplit r

verticalSplit :: Container -> IO (Container, Container)
verticalSplit r@(Container (x,y) w h) = do
  newW <- randomRIO (1, w)
  --print $ "Old w: " ++ show w ++ "New w: " ++ show newW
  let r1 = container (x,y) newW h
      r2 = container ((x+newW),y) (w - newW) h
  checkWRatio r (r1, r2)

horizontalSplit :: Container -> IO (Container, Container)
horizontalSplit r@(Container (x,y) w h) = do
  newH <- randomRIO (1, h)
  -- print $ "Old h: " ++ show h ++ "New h: " ++ show newH
  let r1 = container (x,y) w newH
      r2 = container (x,(y+newH)) w (h-newH)
  checkHRatio r (r1, r2)

checkWRatio :: Container -> (Container,Container) -> IO (Container, Container)
checkWRatio r rs@(Container _ w h , Container _ w1 h1) = 
  let r1 = w / h
      r2 = w1 / h1
  in if r1 < 0.5 || r2 < 0.45
     then randomSplit r
     else return rs

checkHRatio :: Container -> (Container,Container) -> IO (Container, Container)
checkHRatio r rs@(Container _ w h , Container _ w1 h1) = 
  let r1 = h / w
      r2 = h1 / w1
  in if r1 < 0.3 || r2 < 0.45
     then randomSplit r
     else return rs

{- Rooms -}
type Room = Container

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

drawRoom :: Room -> Picture
drawRoom (Container (x,y) w h) =  color (greyN 0.5) $ translate (x+(w/2)) (y+(h/2)) $ rectangleSolid w h

drawRooms :: [Room] -> [Picture]
drawRooms r = map drawRoom r

 
