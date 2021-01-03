{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Animation where


import           Control.Lens                   hiding (element, (#))
import           Control.Lens.Combinators
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map                       as M
import qualified Debug.Trace                    as D
import           Util





type Time = Double

data StartTime = Normal | Delay Double deriving (Show)

data AnimationOptions = AnimationOptions { _delay             :: StartTime
                                         , _animationDuration :: Double
                                         } deriving (Show)

data Animation a = Animation { _animation        :: Time -> a
                             , _animationOptions :: AnimationOptions
                             }

instance Show (Animation a) where
    show (Animation _ o) = "Animation: " ++ show o

data Scene a = Scene { _animations :: [Animation a] } deriving (Show)

data AnimationState a = AnimationState { _scenes :: [Scene a]
                                       } deriving (Show)

type Animator a = State (AnimationState a) ()


$(makeLenses ''Animation)
$(makeLenses ''AnimationOptions)
$(makeLenses ''Scene)
$(makeLenses ''AnimationState)

play :: Monoid a => (Time -> a) -> Animator a
play x = play' x (AnimationOptions Normal 1)

play' :: Monoid a => (Time -> a) -> AnimationOptions -> Animator a
play' x o = do
    scenes %= (Scene [Animation x o]:)

fork :: Monoid a => (Time -> a) -> Animator a
fork x = fork' x (AnimationOptions Normal 1)

fork' :: Monoid a => (Time -> a) -> AnimationOptions -> Animator a
fork' x o = do
    modify (\ s -> s & scenes . element 0 . animations %~ (Animation x o :))

playAnimation :: forall a. Monoid a => Int -> Double -> Animator a -> [a]
playAnimation fps duration s =
    let aState = execState s (AnimationState [])
        getScenes = aState ^.. scenes . traversed
        numberOfFramesInScene :: Scene a -> Double
        numberOfFramesInScene scene =
            (duration / fromIntegral (lengthOf folded getScenes)) * fromIntegral fps * (getSceneDuration scene / sumOfDurations)
        getSceneDuration :: Scene a -> Double
        getSceneDuration scene = maximum (scene ^.. animations . folded . animationOptions . animationDuration)
        sumOfDurations :: Double
        sumOfDurations = sum . map getSceneDuration $ getScenes
        playScene :: Monoid a => Scene a -> [a]
        playScene x = map (\ t -> mconcat (map (\ a -> a t) (x ^.. animations . folded . animation))) [0.001, (1 / (D.trace (show $ numberOfFramesInScene x) (numberOfFramesInScene x))) .. 1]
    in concatMap playScene (D.trace (show getScenes) getScenes)


data SceneState a = SceneState { _currentFrame :: a, _frames :: M.Map Int a, _currentFrameIndex :: Int }

$(makeLenses ''SceneState)

animate :: Monoid a => Int -> [Scene a] -> [a]
animate fps scenes =
    let (SceneState _ frames _) = execState (animateScenes fps scenes) (SceneState mempty M.empty 0)
    in M.elems frames

animateScenes :: Monoid a => Int -> [Scene a] -> State (SceneState a) ()
animateScenes fps scenes = do
    mapM_ animateScene scenes
    where
        animateScene scene = do
            endFrames <- mapM animateAnimation $ scene ^.. animations . folded
            currentFrameIndex .= maximum endFrames
        animateAnimation (Animation a options) = do
            i <- use currentFrameIndex
            let animationFrames = [i .. i + (round (options ^. animationDuration) * fps)]
            mapM_ (\ (t, frame) -> do
                frames . at frame %= Just . maybe (a t) (a t <>)
                currentFrame %= (a 1 <>)) $ zip [0.001, (1 / fromIntegral (length animationFrames)) .. 1] animationFrames
            return $ last animationFrames

