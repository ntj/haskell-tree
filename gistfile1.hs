-- tree.hs
module Main where
  import Graphics.Rendering.Cairo
  import Canvas
  import System.Random

  main = do
    gen <- getStdGen
    let ns = randoms gen :: [Double]
    canvas (draw ns) 600 600

  draw ns w h t = do
    color white
    rectangle 0 0 w h
    fill
    color black
    drawTree ns w h t

  drawTree ns w h t = do
    translate (w/2) (h+5)
    mapM_ strokeWidthLine tree
    where tree = map (mapWidthLine (uscaleP 25)) $ branch ns 8 (pi/2*sin t)

  branch _ 0 _ = []
  branch (r1:r2:rs) n angle =
    (thickness, points) : subBranches
    where
      da = angularDistance 0 angle
      scale = r2 * 5 * ((1-(abs da / pi)) ** 2)
      points = map (rotateP (angle + r1 * da) . uscaleP scale) [(0,0), (0, -1)]
      thickness = n
      [_,(x,y)] = points
      subBranches = map (mapWidthLine (translateP x y)) (left ++ right)
      left = branch (takeOdd rs) (n-1) (angle-r1*pi/4)
      right = branch (takeEven rs) (n-1) (angle+r2*pi/4)

  takeOdd [] = []
  takeOdd [x] = []
  takeOdd (_:x:xs) = x : (takeOdd xs)

  takeEven [] = []
  takeEven [x] = [x]
  takeEven (x:_:xs) = x : (takeEven xs)
