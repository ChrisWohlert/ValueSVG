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

data PercentageCircleSettings = PercentageCircleSettings { _percentage :: Percentage, _color :: Colour Double }

instance Default PercentageCircleSettings where
    def  = PercentageCircleSettings (Percentage 60) purple

$(makeLenses ''PercentageCircleSettings)

dev = renderSVG "test.svg" (mkWidth 400) $ percentageCircle (Percentage 30) # frame 0.1

percentageCircle :: Percentage -> Diagram B
percentageCircle (Percentage p) =
    text (show p ++ "%") # scale 0.4
                         # translateY (-0.05)
    <> dial (Percentage p)

percentageCircle' settings =
    ( text ( ++ "%")) # scale 0.4
                         # translateY (-0.05)
    <> dial (Percentage p)

dial (Percentage p) = (annularWedge 1 width (rotateBy (1/4) xDir) (360 @@ deg)
    <> annularWedge 1 width (rotateBy (1/4) xDir) ((-(360 / 100 * p)) @@ deg))
        # fc purple
        # lc purple
        # opacity 0.3
    where
        width = 0.8
