module Main where

import Plants.SVG
import Control.Monad (forM_)
import Control.Lens (set)

import Systems.Geometric

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

main :: IO ()
main = do
  renderGeometrics
