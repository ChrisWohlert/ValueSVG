{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module ValueSVG where

import           Control.Lens         hiding (element, (#))
import           Diagrams.Backend.SVG
import           Diagrams.Prelude

newtype Percentage = Percentage { unPercentage :: Double }

$(makeLenses ''Percentage)

dev = renderSVG "test.svg" (mkWidth 800) $ percentageHalfCircle (Percentage 30) # frame 0.1

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
percentageHalfCircle (Percentage p) = 
    text (show p ++ "%") # scale 0.4
                         # translateY (-0.05)
    <> halfDial' (Percentage p) # fc purple
                               # lc purple
                               # opacity 0.5

halfDial (Percentage p) = annularWedge 1 width (rotate (315 @@ deg) xDir) (270 @@ deg)
    <> annularWedge 1 width (rotate (225 @@ deg) xDir) ((-(270 / 100 * p)) @@ deg)
    <> wedge 0.1 (rotate (225 @@ deg) xDir) (180 @@ deg)
        # rotate (224.5 @@deg)
        # moveTo (p2 (0, 0.9))
        # rotateAround (p2 (0, 0)) (135.5 @@ deg)
    <> wedge 0.1 (rotate (225 @@ deg) xDir) (180 @@ deg)
        # rotate (45.5 @@deg)
        # moveTo (p2 (0, 0.9))
        # rotateAround (p2 (0, 0)) (224.5 @@ deg)
        # opacity 1.5
    where
        width = 0.8

halfDial' (Percentage p) = annularWedge 1 width (rotate (315 @@ deg) xDir) (270 @@ deg)
    <> annularWedge 1 width (rotate (225 @@ deg) xDir) ((-(270 / 100 * p)) @@ deg)
    <> wedge 0.1 (rotate (225 @@ deg) xDir) (180 @@ deg)
        # rotate (224.5 @@deg)
        # moveTo (p2 (0, 0.9))
        # rotateAround (p2 (0, 0)) (135.5 @@ deg)
    <> wedge 0.1 (rotate (225 @@ deg) xDir) (180 @@ deg)
        # rotate (45.5 @@deg)
        # moveTo (p2 (0, 0.9))
        # rotateAround (p2 (0, 0)) (224.5 @@ deg)
        # opacity 1.5
    where
        width = 0.8



-- wedge 0.1 (rotate (225 @@ deg) xDir) (180 @@ deg) # translate (V2 (-0.631) (-0.642))