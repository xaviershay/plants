module Plants.SVG where

import Plants.LSystem
import Plants.Turtle

data SVGSettings = SVGSettings {
}

emptySVGSettings = SVGSettings {}

default2d = emptySVGSettings

turtleToSvgPath = undefined

renderSvgToFile :: SVGSettings -> LSystem -> String -> IO ()
renderSvgToFile settings system name = do
  let path = (turtleToSvgPath . interpret . run) system
  return ()
