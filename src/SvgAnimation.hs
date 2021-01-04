module SvgAnimation where


import           Animation
import           Diagrams.Backend.Rasterific
import           Diagrams.Prelude            hiding (Time)


mkGif :: Animator (Diagram B) -> IO ()
mkGif anim = animatedGif "test.gif" (mkWidth 400) LoopingNever 2 (playAnimation 50 anim)
