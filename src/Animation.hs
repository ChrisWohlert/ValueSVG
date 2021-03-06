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
import           Data.List
import qualified Data.Map                       as M
import qualified Debug.Trace                    as D
import           Util
import Signal






type Time = Double

data StartTime = Normal | Delay Double deriving (Show)
data Duration = Duration Double | Static deriving (Show)

data AnimationOptions = AnimationOptions { _delay    :: StartTime
                                         , _duration :: Duration
                                         , _signal   :: Signal
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



data SceneState a = SceneState { _currentFrame :: a, _frames :: M.Map Int a, _currentFrameIndex :: Int } deriving (Show)

$(makePrisms ''StartTime)
$(makePrisms ''Duration)
$(makeLenses ''SceneState)
$(makeLenses ''Animation)
$(makeLenses ''AnimationOptions)
$(makeLenses ''Scene)
$(makeLenses ''AnimationState)

play :: Monoid a => (Time -> a) -> Animator a
play x = play' x withOptions

play' :: Monoid a => (Time -> a) -> AnimationOptions -> Animator a
play' x o = scenes %= (Scene [Animation x o]:)

fork :: Monoid a => (Time -> a) -> Animator a
fork x = fork' x withOptions

fork' :: Monoid a => (Time -> a) -> AnimationOptions -> Animator a
fork' x o = scenes . element 0 . animations %= (Animation x o :)

static x = static' x withOptions
static' x o = play' (const x) $ o & duration .~ Static

withOptions = AnimationOptions Normal (Duration 1) linearSignal

getFinalFrame :: Monoid a => Animator a -> a
getFinalFrame x = last $ playAnimation 1 x

playAll xs = playAll' xs withOptions

playAll' :: Monoid a => [Time -> a] -> AnimationOptions -> Animator a
playAll' [] _ = pure ()
playAll' (x:xs) o = do
    play' x o
    playAll' xs o

forkAll xs = forkAll' xs withOptions
forkAll' [] _ = pure ()
forkAll' (x:xs) o = do
    fork' x o
    forkAll' xs o

forkAnims :: [Animator a] -> Animator a
forkAnims as =
    let foldedScenes = map (Scene . concatMap _animations) . transpose . map (\ as -> _scenes $ execState as (AnimationState [])) $ as
    in scenes %= (foldedScenes ++)

playAnimation :: forall a. Monoid a => Int -> Animator a -> [a]
playAnimation fps s =
    let aState = execState s (AnimationState [])
        getScenes = reverse $ aState ^.. scenes . traversed
    in animate fps getScenes

animate :: (Monoid a) => Int -> [Scene a] -> [a]
animate fps scenes =
    let (SceneState _ frames _) = execState (animateScenes fps scenes) (SceneState mempty M.empty 0)
    in M.elems $ D.trace (show $ M.size frames) frames

animateScenes :: Monoid a => Int -> [Scene a] -> State (SceneState a) ()
animateScenes fps scenes = do
    mapM_ animateScene scenes
    where
        animateScene scene = do
            endFrames <- mapM animateAnimation . reverse $ scene ^.. animations . folded
            let lastFrameOfScene = maximum . map snd $ endFrames
            mapM_ (makeLastFrameOfAnimationStick lastFrameOfScene) endFrames
            currentFrameIndex .= lastFrameOfScene
        animateAnimation ani@(Animation a options) = do
            animationFrames <- getAnimationFrames options
            mapM_ (\ (t, frame) -> insertAnimation (a t) frame) animationFrames
            currentFrame %= (a 1 <>)
            return (ani, snd . last $ animationFrames)
        insertAnimation a f = do
            c <- use currentFrame
            frames . at f %= Just . maybe (a <> c) (a <>)
        getAnimationFrames options = do
            i <- use currentFrameIndex
            let frames = case options ^. duration of
                            Duration d -> zip (options ^. signal <$> [0.0001, (1 / (d * fromIntegral fps)) .. 1]) [i ..]
                            Static -> [(1, i)]
            return $ case options ^. delay of
                        Normal  -> frames
                        Delay d -> frames & traversed . _2 %~ (+ round (d * fromIntegral fps))
        makeLastFrameOfAnimationStick endOfScene (Animation a _, endOfAnimation) =
            mapM_ (insertAnimation $ a 1) [endOfAnimation + 1 .. endOfScene]


(<#>) :: Monoid a => (Time -> a) -> (a -> a) -> (Time -> a)
a <#> b = b . a

(<##>) :: (Monoid a) => (Time -> a) -> (Time -> a -> a) -> (Time -> a)
a <##> f = \ t -> f t (a t)
