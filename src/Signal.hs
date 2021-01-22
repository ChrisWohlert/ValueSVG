{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Signal where

import           Diagrams.TwoD.Types
import           Diagrams.Prelude              hiding (Line, duration,
                                                lineWidth, p3)
import Prelude hiding (log)
import Util

type Signal = Double -> Double

instance Show Signal where
    show s = "Signal " ++ show (s 0.5)


linearSignal = id


p0 :: P2 Double
p0 = 0 ^& 0

p3 :: P2 Double
p3 = 1 ^& 1

cubicBezierS' :: (P2 Double, P2 Double) -> Signal
cubicBezierS' (p1, p2) t = ((6 *(1 - t)) *^ (p2 ^-^ (2 *^ p1) + p0) ^+^ ((6 * t) *^ (p3 ^-^ (2*^p2) ^+^p1))) ^. _y




cubicBezierS :: (P2 Double, P2 Double) -> Signal
cubicBezierS (p1, p2) t = 
    let 
        q0 = p0 .+^ ((p1 .-. p0) ^* t)
        q1 = p1 .+^ ((p2 .-. p1) ^* t)
        q2 = p2 .+^ ((p3 .-. p2) ^* t)
        r0 = q0 .+^ ((q1 .-. q0) ^* t)
        r1 = q1 .+^ ((q2 .-. q1) ^* t)
        b  = r0 .+^ ((r1 .-. r0) ^* t)
    in
        log b ^. _y