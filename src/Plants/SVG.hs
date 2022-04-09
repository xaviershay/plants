{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Plants.SVG where

import Plants.LSystem hiding ((<|), (|>))
import Plants.Prelude
import Plants.Turtle

import Control.Lens (assign, makeLenses, modifying, view)

import Control.Monad (forM_, when)
import Control.Monad.RWS (execRWST, get, tell)
import Data.Foldable (toList)
import Data.List (intercalate, sortOn)
import Data.Sequence (ViewR(..), (|>), viewr)
import qualified Data.Sequence as S

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
  , _pathPoints :: S.Seq ProjectedPoint
  , _pathStroke :: Int
  , _pathStrokeWidth :: Double
  , _pathFill :: Maybe Int
  , _pathZs :: [Double]
  } deriving (Show, Eq)

makeLenses ''SVGPath

mkSVGPath start =
  SVGPath
    { _pathStart = start
    , _pathPoints = mempty
    , _pathStroke = 0
    , _pathStrokeWidth = 1
    , _pathFill = Nothing
    , _pathZs = mempty
    }

avgZ path =
  let zs = view pathZs path
   in case zs of
        [] -> 0.0
        _ -> sum zs / (fromIntegral $ length zs)

projection3to2 matrix p =
  let (V3 x y _) = matrix !* p
   in (V2 x y)

-- Flip Y axis such that positive direction is up rather than down
orthoMatrix = V3 (V3 1.0 0 0) (V3 0.0 (-1) 0) (V3 0.0 0 0)

orthoProjection = projection3to2 orthoMatrix

-- First transform the X/Y axis to point R/U (rather than default R/D in SVG
-- coords), then apply standard iso projection rotations.
isoMatrix = rotateH beta !*! rotateL (pi / 4) !*! rotateL pi !*! rotateU pi

beta = asin (tan (30.0 / 180.0 * pi))

isoProjection = projection3to2 isoMatrix

perspectiveProjection p =
  let (V3 x y w) = cameraRMatrix !* (cameraP - p)
   in (V2 (x / w) (y / w))

cameraP = V3 0 0 100

cameraR = V3 0 0 0

cameraE = V3 0 0 5

cameraEMatrix =
  let (V3 ex ey ez) = cameraE
   in V3 (V3 1 0 (ex / ez)) (V3 0 1 (ey / ez)) (V3 0 0 (1 / ez))

cubePoints = [V3 1.0 1 0, V3 1 1 (-1), V3 (-1) 1 0, V3 (-1) 1 (-1)]

cameraRMatrix =
  let (V3 h u l) = cameraR
  -- cameraEMatrix !*! rotateH l !*! rotateL u !*! rotateU h !*! rotateL pi !*!
  --    rotateU pi
   in cameraEMatrix !*! rotateL pi !*! rotateU pi

defaultStrokeWidth = 0.1

defaultColors :: [String]
defaultColors =
  ["#50514F", "#109648", "#CB793A", "#ffffff", "#8D91C7", "#000000", "#ffff00"]

emptySVGSettings =
  SVGSettings
    { _settingProjection = orthoProjection
    , _settingViewport = ViewportBoundingRect 0.1
    , _settingColors = defaultColors
    , _settingStrokeWidth = defaultStrokeWidth
    , _settingOutputDir = "output/"
    , _settingBackground = "#CBD4C2"
    }

default2d = emptySVGSettings
default3d = set settingProjection perspectiveProjection emptySVGSettings

turtleToSVGPaths :: SVGSettings -> [Instruction] -> [SVGPath]
turtleToSVGPaths settings is =
  snd $ execRWST f () (mkSVGPath (project $ V3 0 0 0)) []
  where
    project = view settingProjection settings
    f = do
      forM_ (is <> [MovePenUp (V3 0 0 0)]) $ \i -> do
        pathUnderConstruction <- get
        let pathStarted = not . null . view pathPoints $ pathUnderConstruction
        case i of
          MovePenDown p
           -> do
            modifying pathPoints (\ps -> ps |> project p)
            modifying
              pathZs
              (\zs ->
                 zs <>
                 [ let (V3 _ _ z) = p
                    in z
                 ])
          _ -> do
            when pathStarted $ do
              tell [pathUnderConstruction]
              assign pathStart $
                case viewr . view pathPoints $ pathUnderConstruction of
                  _ :> lastPoint -> lastPoint
                  _ -> view pathStart pathUnderConstruction
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
              forM_ (sortOn avgZ paths) $ \path -> do
                S.path ! A.style (S.toValue $ toStyle settings path) !
                  A.d
                    (mkPath $ do
                       let (V2 x y) = view pathStart path
                       m x y
                       forM_ (view pathPoints path) $ \(V2 x y) -> l x y)

renderSvgToFile :: SVGSettings -> LSystem -> String -> IO ()
renderSvgToFile settings system name = do
  let contents =
        (pathsToSVG settings . turtleToSVGPaths settings . centreAndScale 100 . interpret . run)
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
pathAllPoints path = view pathStart path : toList (view pathPoints path)

bounds paths =
  let allPoints = concatMap pathAllPoints paths
      xs = map (\(V2 x _) -> x) allPoints
      ys = map (\(V2 _ y) -> y) allPoints
   in if null xs || null ys
        then (V2 0 0, V2 0 0)
        else (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
