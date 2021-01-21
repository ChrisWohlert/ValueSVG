{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NegativeLiterals          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module SvgAnimation (mkGif, drawTrail, drawTrailAt, playTrail, getTrail, getTrailAt) where


import           Animation
import qualified Debug.Trace                 as D
import           Diagrams.Backend.Rasterific
import           Diagrams.Direction
import           Diagrams.Path
import           Diagrams.Prelude            hiding (Time)



mkGif :: Animator (Diagram B) -> IO ()
mkGif anim = animatedGif "test.gif" (mkWidth 800) LoopingNever 2 (playAnimation 50 anim)


drawTrail :: Trail V2 Double -> Time -> Diagram B
drawTrail trail t = strokeTrail $ getTrail trail t

drawTrailAt :: P2 Double -> Trail V2 Double -> Time -> Diagram B
drawTrailAt p trail t = strokeLocTrail $ getTrailAt p trail t

getTrail :: Trail V2 Double -> Time -> Trail V2 Double
getTrail trail t = fromSegments (cutTrailAtTimeDistance t $ trailSegments trail)

getTrailAt :: P2 Double -> Trail V2 Double -> Time -> Located (Trail V2 Double)
getTrailAt p trail t = getTrail trail t `at` p

sumTrailDistance = foldr ((+) . stdArcLength) 0

cutTrailAtTimeDistance t xs =
    let
        totalD = sumTrailDistance xs * t
    in
        foldl (\ acc a ->
            if sumTrailDistance (a:acc) <= totalD
                then acc ++ [a]
                else acc ++ [adjust a (with & adjSide .~ End
                                            & adjMethod .~ ToAbsolute (maximum [0.001, totalD - sumTrailDistance acc]))]) [] xs


playTrail = play . drawTrail
