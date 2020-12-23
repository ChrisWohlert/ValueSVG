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
import           Diagrams.Backend.SVG
import           Diagrams.Prelude

newtype Percentage = Percentage { _percentage :: Double }

data Bar = Bar { _barValue :: Double, _barLabel :: String }

$(makeLenses ''Percentage)
$(makeLenses ''Bar)

dev = renderSVG "test.svg" (mkWidth 400) $ barChart # frame 0.1

percentageCircle :: Percentage -> Diagram B
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

percentageHalfCircle :: Percentage -> Diagram B
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

roundedDial :: Double -> Diagram B
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


barChart :: Diagram B
barChart =
       hsep 0.1 (strutX 0 : bars [Bar 0.5 "Test", Bar 0.2 "Test2"])
    <> strutY 0.2
    <> dashedBackground
        # translateY 1
        # opacity 0.05

dashedBackground :: Diagram B
dashedBackground = vsep 0.1 (replicate 10 (fromVertices (map p2 [(0, 0), (1, 0)]) # dashingN [0.01, 0.01] 0.01))

colors = concatMap (`brewerSet` 9) [Pastel1, Pastel2, Set1, Set2, Set3, Paired]

bars :: [Bar] -> [Diagram B]
bars bs = zipWith mkBar bs colors

mkBar bar color =
    roundedRect' 0.1 (bar ^. barValue) (with & radiusTL .~ 0.01
                                             & radiusTR .~ 0.01)
        # translateY (bar ^. barValue / 2)
        # fillTexture gradient
        # lineTexture gradient
    <> topLeftText (bar ^. barLabel)
        # font "Tahoma"
        # fc color
        # scale 0.05
        # rotateBy (-1/8)
    <> text (show $ bar ^. barValue)
        # translateY (0.4 + 20 * bar ^. barValue)
        # scale 0.05
        # fc color
    where
        gradient = mkLinearGradient (mkStops [(white, 0, 1), (color, 1, 1)]) (0 ^& (-0.3)) (0 ^& bar ^. barValue) GradPad

yAxis = strokeLine $ fromVertices [0 ^& 0, 0 ^& 1]

xAxis = strokeLine $ fromVertices [0 ^& 0, 1 ^& 0]
