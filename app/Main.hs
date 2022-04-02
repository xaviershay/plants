module Main where

import Plants.SVG
import Plants.LSystem.Eval
import Plants.LSystem.Parser
import Control.Monad (forM_)
import Control.Lens (set)
import Linear (V2(..))

import Systems.Geometric
import Systems.Plants2D
import Systems.Penrose

main :: IO ()
main = do
  putStrLn $ show testP
  -- renderGeometrics
  -- renderPlants2D
  -- renderPenrose

renderGeometrics = do
  let systems =
        [ ("koch-tiles", kochTiles)
        , ("koch-spiral", kochSpiral)
        , ("koch-island", kochIsland)
        , ("island-lakes", islandLakes)
        , ("dragon-curve", dragonCurve)
        , ("gosper-hex-curve", gosperHexCurve)
        ]
  forM_ systems $ \(name, system) -> do
    renderSvgToFile (set settingOutputDir "output/geometric-" default2d) system name

renderPlants2D = do
  let systems = map (\n -> ("branching-" <> show n, branching n)) [1..6]
  forM_ systems $ \(name, system) -> do
    renderSvgToFile (set settingOutputDir "output/plants-2d-" default2d) system name

renderPenrose = do
  renderSvgToFile (
    set settingViewport (ViewportFixed (V2 (-5) (-5), V2 5 5))
    . set settingStrokeWidth 0.03
    $ default2d
    ) penroseStencil "penrose-stencil"

  -- Colors from https://coolors.co/ef476f-ffd166-b4e7f8-7cd5f3-e9e9ed
  renderSvgToFile  (
    set settingViewport (ViewportBoundingRect 0.05)
    . set settingStrokeWidth 0.01
      . set settingBackground "#7CD5F3"
      . set settingColors
        [ "#000000"
        , "#EF476F"
        , "#FFD166"
        ]
    $ default2d)
    penrose "penrose"
