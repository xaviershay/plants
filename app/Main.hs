module Main where

import Plants.SVG (default2d, renderSvgToFile)

import Systems.Geometric

main :: IO ()
main = do
  renderSvgToFile default2d kochTiles "koch-tiles"
  renderSvgToFile default2d kochSpiral "koch-spiral"
