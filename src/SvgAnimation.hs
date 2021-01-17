module SvgAnimation where


import           Animation
import qualified Debug.Trace                 as D
import           Diagrams.Backend.Rasterific
import           Diagrams.Direction
import           Diagrams.Path
import           Diagrams.Prelude            hiding (Time)



mkGif :: Animator (Diagram B) -> IO ()
mkGif anim = animatedGif "test.gif" (mkWidth 400) LoopingNever 2 (playAnimation 50 anim)



drawFromVertices :: [Point V2 Double] -> Time -> Diagram B
drawFromVertices b t = strokePath . fromOffsets . cutAtTimeDistance t . makeVector . fromVertices $ b

makeVector :: [Point V2 Double] -> [V2 Double]
makeVector xs = zipWith (.-.) xs (tail xs)

takePart :: Time -> [a] -> [a]
takePart t xs = take (round $ fromIntegral (length xs) * t) xs

sumDistance = foldr ((+) . norm) 0

cutAtTimeDistance :: Double -> [V2 Double] -> [V2 Double]
cutAtTimeDistance t xs =
    let
        totalD = sumDistance xs * t
    in
        foldl (\ acc a -> if sumDistance (a:acc) <= totalD then acc ++ [a] else acc ++ [normalize a ^* (totalD - sumDistance acc)]) [] xs

scaleLastVertice t [] = []
scaleLastVertice t xs = init xs ++ [last xs ^* t]
