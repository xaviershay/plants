{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Plants.SVG where

import Plants.LSystem
import Plants.Prelude
import Plants.Turtle

import Control.Lens (assign, makeLenses, modifying, view)

import Control.Monad (forM_, when)
import Control.Monad.RWS (execRWST, get, tell)
import Data.List (intercalate)

import Linear (V2(..), V3(..), (!*), (!*!))

import Text.Blaze.Svg11 ((!), l, lr, m, mkPath, mr, rotate)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

--import Text.Blaze.Svg.Renderer.String (renderSvg)
import Text.Blaze.Svg.Renderer.Pretty (renderSvg)

type ProjectedPoint = V2 Double

data Viewport
  = ViewportFixed (ProjectedPoint, ProjectedPoint)
  | ViewportBoundingRect Double

data SVGSettings = SVGSettings
  { _settingProjection :: Point -> ProjectedPoint
  , _settingViewport :: Viewport
  , _settingColors :: [String]
  , _settingBackground :: String
  , _settingStrokeWidth :: Double
  , _settingOutputDir :: String
  }

makeLenses ''SVGSettings

data SVGPath = SVGPath
  { _pathStart :: ProjectedPoint
  , _pathPoints :: [ProjectedPoint]
  , _pathStroke :: Int
  , _pathStrokeWidth :: Double
  , _pathFill :: Maybe Int
  } deriving (Show, Eq)

makeLenses ''SVGPath

mkSVGPath start =
  SVGPath
    { _pathStart = start
    , _pathPoints = mempty
    , _pathStroke = 0
    , _pathStrokeWidth = 1
    , _pathFill = Nothing
    }

orthoProjection = V3 (V3 1.0 0 0) (V3 0.0 (-1) 0) (V3 0.0 0 0)

defaultStrokeWidth = 0.1

defaultColors :: [String]
defaultColors =
  ["#50514F", "#109648", "#CB793A", "#ffff00", "#8D91C7", "#000000", "#ffff00"]

emptySVGSettings =
  SVGSettings
    { _settingProjection =
        \p ->
          let (V3 x y _) = orthoProjection !* p
           in (V2 x y)
    , _settingViewport = ViewportBoundingRect 0.1
    , _settingColors = defaultColors
    , _settingStrokeWidth = defaultStrokeWidth
    , _settingOutputDir = "output/"
    , _settingBackground = "#CBD4C2"
    }

default2d = emptySVGSettings

turtleToSVGPaths :: SVGSettings -> [Instruction] -> [SVGPath]
turtleToSVGPaths settings is = snd $ execRWST f () (mkSVGPath (V2 0 0)) []
  where
    project = view settingProjection settings
    f = do
      forM_ (is <> [MovePenUp (V3 0 0 0)]) $ \i -> do
        pathUnderConstruction <- get
        let pathStarted = not . null . view pathPoints $ pathUnderConstruction
        case i of
          MovePenDown p
           -- TODO: Use sequence instead
           -> do
            modifying pathPoints (\ps -> ps <> [project p])
          _ -> do
            when pathStarted $ do
              tell [pathUnderConstruction]
              -- TODO: Handle when path has no points
              assign
                pathStart
                (head . reverse . view pathPoints $ pathUnderConstruction)
              assign pathPoints mempty
            case i of
              MovePenUp x -> assign pathStart . project $ x
              ChangeColor x -> assign pathStroke x
              StrokeWidth x -> assign pathStrokeWidth x
              Fill x -> assign pathFill x

pathsToSVG :: SVGSettings -> [SVGPath] -> String
pathsToSVG settings paths = renderSvg svgDoc
  where
    svgDoc :: S.Svg
    svgDoc =
      let (V2 minX minY, V2 maxX maxY) =
            case view settingViewport settings of
              ViewportBoundingRect n -> extrude n (bounds paths)
              ViewportFixed x -> x
          aspect = (maxX - minX) / (maxY - minY)
       in S.docTypeSvg ! A.version "1.1" ! A.width (S.toValue $ 500 * aspect) !
          A.height "500" !
          A.viewbox
            (S.toValue . intercalate " " . map show $
             [minX, minY, maxX - minX, maxY - minY]) $ do
            S.g $ do
              S.rect ! A.x (S.toValue minX) ! A.y (S.toValue minY) !
                A.width (S.toValue $ maxX - minX) !
                A.height (S.toValue $ maxY - minY) !
                (A.fill . S.toValue $ view settingBackground settings)
              forM_ paths $ \path -> do
                S.path ! A.style (S.toValue $ toStyle settings path) !
                  A.d
                    (mkPath $ do
                       let (V2 x y) = view pathStart path
                       m x y
                       forM_ (view pathPoints path) $ \(V2 x y) -> l x y)

renderSvgToFile :: SVGSettings -> LSystem -> String -> IO ()
renderSvgToFile settings system name = do
  let contents =
        (pathsToSVG settings . turtleToSVGPaths settings . interpret . run)
          system
  writeFile (view settingOutputDir settings <> name <> ".svg") contents

extrude ::
     Double
  -> (ProjectedPoint, ProjectedPoint)
  -> (ProjectedPoint, ProjectedPoint)
extrude t (V2 x1 y1, V2 x2 y2) =
  let sx = (x2 - x1) * t
      sy = (y2 - y1) * t
   in (V2 (x1 - sx) (y1 - sy), V2 (x2 + sx) (y2 + sy))

toStyle :: SVGSettings -> SVGPath -> String
toStyle settings path =
  intercalate ";" . map (\(a, b) -> a <> ":" <> b) $
  [ ("fill", maybe "none" toRGB $ view pathFill path)
  , ("stroke", toRGB $ view pathStroke path)
  , ( "stroke-width"
    , showFullPrecision
        (view pathStrokeWidth path * (view settingStrokeWidth settings)) <>
      "px")
  , ("stroke-linecap", "square")
  ]
  where
    cs = view settingColors settings
    toRGB n = cs !! (n `mod` length cs)

pathAllPoints :: SVGPath -> [ProjectedPoint]
pathAllPoints path = view pathStart path : view pathPoints path

bounds paths =
  let (mn, mx) =
        foldl
          (\((V2 minX minY), (V2 maxX maxY)) pos ->
             let (V2 x y) = pos
              in (V2 (min minX x) (min minY y), V2 (max maxX x) (max maxY y)))
          (V2 0 0, V2 0 0)
          (concatMap pathAllPoints paths)
   in (mn, mx)
