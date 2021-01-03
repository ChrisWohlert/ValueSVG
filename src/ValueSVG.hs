{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module ValueSVG where

import           Control.Lens                  hiding (element, (#))
import           Data.Colour.Palette.BrewerSet
import qualified Debug.Trace                   as D
import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.SVG
import           Diagrams.Prelude              hiding (Line)
import           SvgAnimation
import           Util

type Coordinate = (Double, Double)

newtype Percentage = Percentage { _percentage :: Double }

data Bar = Bar { _barValue :: Double, _barLabel :: String }
data BarSetting = BarSetting { _barWidth :: Double }

data Pie = Pie { _pieValue :: Double, _pieLabel :: String }

data Line = Line { _lineValues :: [Coordinate], _lineLabel :: String }
data LineSettings = LineSettings { _lineWidth :: Double }

$(makeLenses ''Percentage)
$(makeLenses ''Bar)
$(makeLenses ''BarSetting)
$(makeLenses ''Pie)
$(makeLenses ''Line)
$(makeLenses ''LineSettings)

dev = do
    svg
    gif

svg = renderSVG "test.svg" (mkWidth 400) $ lineGraph (LineSettings 0.3) [Line [(1, 6), (4, 3), (7, 1), (10, 7)] "Test"] # bgFrame 0.1 white

gif = animatedGif "test.gif" (mkWidth 400) LoopingForever 0 anim
    where
        anim = map (rect 4 4 <>) [ lineGraph (LineSettings 0.3) [Line [(1, 2), (4, 3), (7, 1), (10, 7)] "Test"] # bgFrame 0.1 white
               , lineGraph (LineSettings 0.3) [Line [(1, 3), (4, 1), (7, 6), (10, 7)] "Test"] # bgFrame 0.1 white]

percentageCircle (Percentage p) =
    text (show p ++ "%") # scale 0.4
                         # translateY (-0.05)
    <> dial (Percentage p) # fc purple
                           # lc purple
                           # opacity 0.5

dial (Percentage p) = annularWedge 1 width (rotateBy (1/4) xDir) (360 @@ deg)
    <> annularWedge 1 width (rotateBy (1/4) xDir) ((-(360 / 100 * p)) @@ deg)
    where
        width = 0.8

percentageHalfCircle p =
    text (show (p ^. percentage) ++ "%") # scale 0.4
                                         # translateY (-0.05)
    <> halfDial p # fc purple
                  # lc purple

halfDial p =
        roundedDial width
    <>  roundedDialNeedle p width
    where
        width = 0.8

roundedDial width =
       (annularWedge 1 width (rotate ((-45) @@ deg) xDir) (270 @@ deg)
    <> circle 0.1
        # moveTo (p2 (0, 0.9))
        # rotateAround (p2 (0, 0)) (135.5 @@ deg)
    <> circle 0.1
        # moveTo (p2 (0, 0.9))
        # rotateAround (p2 (0, 0)) ((-135.5) @@ deg))
        # opacityGroup 0.5


roundedDialNeedle p width =
       annularWedge 1 width (rotate (225 @@ deg) xDir) ((-(270 / 100 * (p ^. percentage))) @@ deg)
    <> circle 0.1
        # moveTo (p2 (0, 0.9))
        # rotateAround (p2 (0, 0)) (135.5 @@ deg)
    <> circle 0.1
        # moveTo (p2 (0, 0.9))
        # rotateAround (p2 (0, 0)) ((135.5 - (135.5 * 2 * p ^. percentage / 100)) @@ deg)


barChart settings bs =
       hsep 0.1 (strutX 0 : bars settings bs)
    <> strutY 0.2
    <> dashedBackground 0.1 (settings ^. barWidth * numberOfBars + 0.1 * numberOfBars + 0.1) 10
        # translateY 1
        # opacity 0.4
    where
        numberOfBars = fromIntegral (length bs)

dashedBackground sep w n = vsep sep (replicate n (fromVertices (map p2 [(0, 0), (w, 0)]) # dashingN [0.01, 0.01] 0.01))

colors = cycle $ concatMap (`brewerSet` 9) [Pastel1, Pastel2, Set1, Set2, Set3, Paired]

bars settings bs = zipWith (mkBar (settings ^. barWidth) (maximum (bs ^.. folded . barValue))) bs colors

mkBar width maxValue bar color =
    let normalizedValue = bar ^. barValue / maxValue
    in
        roundedRect' width normalizedValue (with & radiusTL .~ 0.01
                                             & radiusTR .~ 0.01)
            # translateY (normalizedValue / 2)
            # fillTexture (gradient normalizedValue)
            # lineTexture (gradient normalizedValue)
        <> topLeftText (bar ^. barLabel)
            # font "Tahoma"
            # fc color
            # scale 0.05
            # rotateBy (-1/8)
        <> text (show $ bar ^. barValue)
            # translateY (0.4 + 20 * normalizedValue)
            # scale 0.05
            # fc color
    where
        gradient v = mkLinearGradient (mkStops [(white, 0, 1), (color, 0.4, 1)]) (0 ^& (-0.3)) (0 ^& v) GradPad

yAxis = strokeLine $ fromVertices [0 ^& 0, 0 ^& 1]

xAxis = strokeLine $ fromVertices [0 ^& 0, 1 ^& 0]

pieChart pies = pieChart' pies (90 @@ deg) colors
    where
        sumOfValues = sum . map (^. pieValue) $ pies
        pieChart' [] _ _ = mempty
        pieChart' (p:ps) accRatio (c:cs) =
            let ratio = ((-1) * 360 * p ^. pieValue / sumOfValues) @@ deg
                newPie = wedge 1 (rotate accRatio xDir) ratio
                    # fc c
                    # lc c
                    -- # translate (0.1 *^ fromDirection (rotate (accRatio ^+^ ratio  ^/ 2) xDir))
                restPies = pieChart' ps (accRatio ^+^ ratio) cs
                label =
                    rect 0.1 0.1
                        # frame 0.1
                        # translateX 1.3
                        # translateY (fromIntegral (length (p:ps)) / 4 - 0.59)
                        # fc c
                        # lc c
                    ||| topLeftText (p ^. pieLabel)
                        # scale 0.15
                        # translateY (fromIntegral (length (p:ps)) / 4 - 0.5)
            in
                newPie <> restPies <> label <> strutX 4

lineGraph settings lines =
    let lineDrawings = mconcat (zipWith drawLine lines colors)
        bg = dashedBackground (height lineDrawings / 8) 2 10 # translateY 0.75 # opacity 0.4
    in bg <> lineDrawings
    where
        drawLine line color = lc color . strokeTrail . fromVertices . vertices $ line
        vertices = map p2 . normalize
        normalizedValues = concatMap normalize lines
        normalize line = line ^. lineValues & traversed . _Y %~ (/ maxYValue)
                                            & traversed . _X %~ ((/ maxXValue) . (*2.0))
        maxYValue = maximum $ values _Y
        maxXValue = maximum $ values _X
        minYValue = minimum $ values _Y
        values s = concatMap (^.. lineValues . traversed . s) $ lines


_X = _1
_Y = _2
