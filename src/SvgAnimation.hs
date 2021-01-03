module SvgAnimation where


import           Animation
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (Time)


mkGif = animatedGif "test.gif" (mkWidth 400) LoopingForever 1 (playAnimation 10 1 anim)

anim :: Animator (Diagram B)
anim = do
    play $ \ t -> rect 2 2 # frame 1 # bg white
    fork $ \ t -> circle (1 * t)
    fork $ \ t -> circle (0.5 * t)
