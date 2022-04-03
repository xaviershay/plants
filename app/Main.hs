module Main where

import Plants.SVG

import Systems.Geometric
import Systems.Penrose
import Systems.Plants2D

import Control.Lens (set)
import Control.Monad (forM_)
import Data.Time as T
import Linear (V2(..))

main :: IO ()
main = do
  renderGeometrics
  --renderPlants2D
  -- renderHonda
  --renderPenrose

renderGeometrics = do
  let systems =
        [--- ("koch-tiles", kochTiles)
        --, ("koch-spiral", kochSpiral)
        --, ("koch-island", kochIsland)
        --, ("island-lakes", islandLakes)
        --, ("dragon-curve", dragonCurve)
        --, ("gosper-hex-curve", gosperHexCurve)
         ("cube", cube)
        ]
  forM_ systems $ \(name, system) -> do
    renderSvgWithTime
      (set settingProjection isoProjection . set settingOutputDir "output/geometric-" $ default2d)
      system
      name

renderPlants2D = do
  let systems = map (\n -> ("branching-" <> show n, branching n)) [1 .. 6]
  forM_ systems $ \(name, system) -> do
    renderSvgWithTime
      (set settingOutputDir "output/plants-2d-" default2d)
      system
      name

-- TODO: Render these with 3D projection (and move out of 2D system)
renderHonda = do
  let systems = map (\n -> ("honda-" <> show n, honda n)) [1 .. 4]
  forM_ systems $ \(name, system) -> do
    renderSvgWithTime
      (set settingOutputDir "output/plants-2d-" .
       set settingProjection perspectiveProjection $
       default2d)
      system
      name

renderPenrose = do
  renderSvgWithTime
    (set settingViewport (ViewportFixed (V2 (-30) (-30), V2 30 30)) $ default2d)
    penroseStencil
    "penrose-stencil"
  -- Colors from https://coolors.co/ef476f-ffd166-b4e7f8-7cd5f3-e9e9ed
  renderSvgWithTime
    (set settingViewport (ViewportBoundingRect 0.05) .
     set settingBackground "#7CD5F3" .
     set settingColors ["#000000", "#EF476F", "#FFD166"] $
     default2d)
    penrose
    "penrose"

renderSvgWithTime settings system name = do
  putStr name
  start <- T.getCurrentTime
  renderSvgToFile settings system name
  finish <- T.getCurrentTime
  putStrLn $ "\t (" <> show (T.diffUTCTime finish start) <> ")"
