module Plants.SVG where

import Plants.LSystem

data SVGSettings = SVGSettings {
}

emptySVGSettings = SVGSettings {}

default2d = emptySVGSettings

renderSvgToFile :: SVGSettings -> LSystem -> String -> IO ()
renderSvgToFile settings system name = return ()
