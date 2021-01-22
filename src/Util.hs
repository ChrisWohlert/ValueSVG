
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Util where

import           Diagrams.Backend.SVG
import           Diagrams.Prelude     hiding (Line)
import           Fmt

import qualified Debug.Trace          as D

class Optional f a where
    (???) :: f a -> a -> a
    infixr 7 ???

instance Optional Maybe a where
    Just x ??? _  = x
    Nothing ??? x = x


takePart :: Double -> [a] -> [a]
takePart t xs = take (ceiling $ t * fromIntegral (length xs)) xs


log x = D.trace (show x) x
