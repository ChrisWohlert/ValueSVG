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
import Prelude hiding (log)
import           Control.Lens                  hiding (at, element, none, (#))
import           Control.Monad
import           Data.Colour.Palette.BrewerSet
import           Diagrams.Backend.Rasterific
import           Diagrams.CubicSpline.Boehm
import           Diagrams.Prelude              hiding (Line, duration,
                                                lineWidth)
import           Diagrams.Trail                hiding (Line)
import           Fmt
import           Graphics.SVGFonts
import           SvgAnimation
import           Util
import Signal

newtype Percentage = Percentage { _percentage :: Double }

data Bar = Bar { _barValue :: Double, _barLabel :: String }
data BarSetting = BarSetting { _barWidth :: Double }

data Pie = Pie { _pieValue :: Double, _pieLabel :: String }

data Line = Line { _lineValues :: [Double], _lineLabel :: String }
data LineSettings = LineSettings { _lineWidth :: Double, _xAxis :: [String] }

$(makeLenses ''Percentage)
$(makeLenses ''Bar)
$(makeLenses ''BarSetting)
$(makeLenses ''Pie)
$(makeLenses ''Line)
$(makeLenses ''LineSettings)

gif = mkGif lineGraphAnim --dBez
    where
        --barChartAnim = barChart (BarSetting 0.1) [Bar 3 "Test 1", Bar 5 "Test 2", Bar 2 "Test 3", Bar 6 "Test 4"]
        lineGraphAnim = lineGraph (LineSettings 0.008 (map show [-4, -3 .. 4]))
                                                    [ Line [3.5, 10, 1, 0.1, 5, 3, 2, 1, 5] "Test 1"
                                                    , Line [1, 1, 3, 10, 27, 7, 10, 11, 3] "Test 2"]
        tt = do
            static $ rect 2 2 # frame 1
            playAll [\ t -> rect (0.5 * t) 1 # lc white, \ t -> rect (0.7 * t) 1 # lc white, \t -> rect (1 * t) 1 # lc white]
        dTrail = do
            static $ rect 3 3 # fc white
            play' (drawTrail $ fromVertices [0 ^& 0, 1 ^& 1, 1.1 ^& 1.3, 0.2 ^& 1, 1.5 ^& 0.6, 0 ^& 0]) (withOptions & duration . _Duration .~ 3)
        dArc = do
            static $ rect 2 2 # frame 1
            play $ drawTrail (arc (direction (0 ^& 1)) (-180 @@ deg)) <#> lc white
        dCircle = do
            static $ rect 2 2 # frame 1
            play $ drawTrail (circle 1) <#> lc white
        dBez = do
            static $ rect 3 3 # fc white
            static $ zip [0.0001, (1 / 50) .. 1] (cubicBezierS (1 ^& 0, 0 ^& 1) <$> [0.0001, (1 / 50) .. 1])
                        # map p2
                        # fromVertices
                        # strokeLine
        asd = do
            static $ rect 4 4 # bgFrame 1 white
            play' (drawTrail (circle 1)
                <##> (\ t -> translate $ (1.5 ^& -1.5) ^* t))
                (withOptions & duration .~ Duration 2)
            fork $ drawTrail (rect 2 2)
                <#> lc green
                <##> (\ t -> scale (1.5 - t))
            play $ \ t -> circle 1 # fc black # translate (0.5 ^& -1.5) # opacity t
            fork $ drawTrail (cubicSpline False [1 ^& 1, 2 ^& 3, 3 ^& 2, 0.5 ^& 2.7, -1.5 ^& -1, 1 ^& 1, 3 ^& 3, 1 ^& 2, 1 ^& 1])
                <##> (\ t -> lc (last . takePart t $ take 2 colors))
            fork' (drawTrail (pentagon 1.5)
                <#> lc pink
                <##> (\ t -> scale (1 * t)))
                (withOptions & duration . _Duration *~ 0.5
                             & delay .~ Delay 1)
        go = do
            static $ rect 5 5 # fc white
            play $ \ t -> fc yellow . strokeTrail . closeTrail $ cubicSpline False [-2 ^& -2, 1^&1, 2^&1] <> fromVertices [2^&1, 1 ^& 2]

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

dashedBackground sep w n = vsep sep (replicate n (fromVertices (map p2 [(0, 0), (w, 0)]) # dashingN [0.005, 0.005] 0.005))

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
lineGraph settings lines =
    let
        w = 2
        h = 1.4
        drawLine l c = drawTrailAt (0 ^& firstYValueNormalized l) (fromVertices $ vertices l)
                            # lwL (settings ^. lineWidth)
                            # lc c
        drawLineBackground l c t = 
            (fromVertices [(width $ drawTrail (fromVertices $ vertices l) t) ^& 0, 0 ^& 0, 0 ^& firstYValueNormalized l]
            <> getTrail (fromVertices $ vertices l) t)
            # closeTrail
            # stroke
            # fc c
            # translateX (width $ drawTrail (fromVertices $ vertices l) t)
            # lw 0
            # opacity 0.4
        bg = dashedBackground (h / 5) 2 5 # translateY h # opacity 0.2 # lwN 0.0035
        maxYValue = getHighestYValue values
        xValues = [0, (w / fromIntegral (length (settings ^. xAxis) - 1)) .. w] :: [Double]
        firstYValueNormalized = head . normalize
        values = concatMap _lineValues lines
        vertices l = zipWith (curry p2) xValues (normalize l)
        normalize :: Line -> [Double]
        normalize line = map ((h *) . (/ maxYValue)) $ line ^. lineValues
        yLabels = map (\ v -> 
                textSVG (show v) 1
                    # stroke
                    # fc black
                    # scale 0.06
                    # lw 0 
                    # fc white
            ||| strutX 0.02 
            ||| fromOffsets [-0.03 ^& 0] # strokeThinGrayLine) 
            (log [maxYValue, maxYValue - (maxYValue / 5) .. 0])
    in do
        static $ rect (w + 0.2) (h + 0.2) # lw 0 # bgFrame 0.1 white # translateX 1 # translateY (h / 2)
        static bg
        static $ vsep (log ((h - 0.135) / 5)) yLabels # translateX (-0.075) # translateY h
        static $ fromVertices [0 ^& 0, w ^& 0] # strokeThinGrayLine
        static $ fromVertices [0 ^& 0, 0 ^& h] # strokeThinGrayLine
        static $ atPoints (map (^& 0) xValues) (map (\ a ->
            fromOffsets [0 ^& -0.03] # strokeThinGrayLine
            ===
            strutY 0.02
            ===
            textSVG a 1
                # stroke
                # fc black
                # scale 0.06
                # lw 0) $ settings ^. xAxis) # fc white
        forkAll' (zipWith drawLine lines colors) (withOptions & signal .~ cubicBezierS (1 ^& 0, 0 ^& 1)
                                                              & duration . _Duration .~ 3)
        forkAll' (zipWith drawLineBackground lines colors) (withOptions & signal .~ cubicBezierS (1 ^& 0, 0 ^& 1)
                                                                        & duration . _Duration .~ 3)

strokeThinGrayLine = lc lightgray . lwN 0.0035 . strokeLine

getHighestYValue :: [Double] -> Double
getHighestYValue xs = head $ dropWhile (highest >) (values 1.0)
    where
        highest = maximum xs
        values x = map (* x) [1 .. 5] ++ values (x * 10)
