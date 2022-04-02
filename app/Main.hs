module Main where

import Plants.SVG
import Control.Monad (forM_)
import Control.Lens (set)

import Systems.Geometric
import Systems.Plants2D

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

main :: IO ()
main = do
  renderPlants2D
  -- renderGeometrics
