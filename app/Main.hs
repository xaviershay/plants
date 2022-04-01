module Main where

import Plants.SVG (renderSvgToFile)

import Systems.Geometric

main :: IO ()
main = do
  renderSvgToFile default2dSvg kochTiles "koch-tiles"
  renderSvgToFile default2dSvg kochSpiral "koch-spiral"
