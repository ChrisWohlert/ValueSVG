{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NegativeLiterals          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module ValueSVG where

import           Animation
import           Control.Lens                  hiding (element, (#))
import           Control.Monad
import           Data.Colour.Palette.BrewerSet
import qualified Debug.Trace                   as D
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude              hiding (Line)
import           Diagrams.TrailLike
import           Fmt
import qualified SvgAnimation                  as S
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

gif = S.mkGif dTrail
    where
        --barChartAnim = barChart (BarSetting 0.1) [Bar 3 "Test 1", Bar 5 "Test 2", Bar 2 "Test 3", Bar 6 "Test 4"]
        lineGraphAnim = lineGraph (LineSettings 0.1) [ Line [(1, 1), (2, 2), (3, 4), (4, 3)] "Test 1"
                                                     , Line [(1, 3), (2, 1.5), (3, 2), (4, 1)] "Test 1"]
        tt = do
            static $ rect 2 2 # frame 1
            playAll [\ t -> rect (0.5 * t) 1 # lc white, \ t -> rect (0.7 * t) 1 # lc white, \t -> rect (1 * t) 1 # lc white]
        dTrail = do
            static $ rect 3 3 # fc white
            play' (S.drawFromVertices [0 ^& 0, 1 ^& 1, 1.1 ^& 1.3, 0.2 ^& 1, 1.5 ^& 0.6, 0 ^& 0]) (withOptions & animationDuration . _Duration .~ 3)

percentageCircle (Percentage p) =
    textP 0 p # scale 0.4
                         # translateY (-0.05)
    <> dial (Percentage p) # fc purple
                           # lc purple
                           # opacity 0.5

dial (Percentage p) = annularWedge 1 width (rotateBy (1/4) xDir) (360 @@ deg)
    <> annularWedge 1 width (rotateBy (1/4) xDir) ((-(360 / 100 * p)) @@ deg)
    where
        width = 0.8

percentageHalfCircle p =
    textP 0 (p ^. percentage) # scale 0.4
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


barChart settings bs = do
    static $ boundingRect (animateBars 1) # lc white # bgFrame 0.1 white
    play animateBars
    where
        animateBars t = hsep 0.1 (strutX 0 : bars settings bs t)
                     <> strutY 0.2
                     <> dashedBackground 0.1 (settings ^. barWidth * numberOfBars + 0.1 * numberOfBars + 0.1) 10
                         # translateY 1
                         # opacity 0.4
        numberOfBars = fromIntegral (length bs)

dashedBackground sep w n = vsep sep (replicate n (fromVertices (map p2 [(0, 0), (w, 0)]) # dashingN [0.01, 0.01] 0.01))

colors :: [Colour Double]
colors = cycle $ concatMap (`brewerSet` 9) [Set1, Pastel1, Pastel2, Set2, Set3, Paired]

bars settings bs t = zipWith (mkBar t (settings ^. barWidth) (maximum (bs ^.. folded . barValue))) bs colors

mkBar t width maxValue bar color =
    let normalizedValue = bar ^. barValue / maxValue * t
    in
        roundedRect' width normalizedValue (with & radiusTL .~ 0.01
                                                 & radiusTR .~ 0.01)
            # translateY (normalizedValue / 2)
            # fillTexture (gradient normalizedValue)
            # lineTexture (gradient normalizedValue)
        <> topLeftText (bar ^. barLabel)
            # font "Tahoma"
            # fc color
            # translateY (-0.2)
            # scale 0.05
            # rotateBy (-1/8)
        <> textP 1 (bar ^. barValue * t)
            # translateY (0.8 + 20 * normalizedValue)
            # scale 0.05
            # fc color
    where
        gradient v = mkLinearGradient (mkStops [(white, 0, 1), (color, 0.4, 1)]) (0 ^& (-0.3)) (0 ^& v) GradPad

textF = text . fmt

textP d p = textF $ ""+|fixedF d p|+"%"

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

lineGraph :: LineSettings -> [Line] -> Animator (Diagram B)
lineGraph settings lines = do
    static $ rect 2 1.2 # bgFrame 0.1 white # translateX 1 # translateY 0.6
    static bg
    forkAnims $ zipWith animateLine lines colors
        where
            animateLine line color = playAll $
                map (\ (from, to) t -> translateX (-minNormalizedXValue) . moveTo from . lc color . strokeLine . fromVertices $ [from, from .+^ (to .-. from) ^* t]) (linePairs line)
            linePairs line = pairs $ offsets line
            bg = dashedBackground (1.2 / 16) 2 16 # translateY 1.2 # opacity 0.4
            maxYValue = maximum $ values _Y
            maxXValue = maximum $ values _X
            minXValue = minimum $ values _X
            minNormalizedXValue = minimum $ map (minimum . map fst . normalize) lines
            values s = concatMap (^.. lineValues . traversed . s) lines
            pairs xs = zip xs (tail xs)
            vertices = map p2 . normalize
            offsets = map p2 . normalize
            normalize line = line ^. lineValues & traversed . _Y %~ (/ maxYValue)
                                                & traversed . _X %~ ((/ (maxXValue - minXValue)) . (*2.0))


_X = _1
_Y = _2
