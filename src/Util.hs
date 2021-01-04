
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}


module Util where

import           Diagrams.Backend.Rasterific
import           Diagrams.Backend.SVG
import           Diagrams.Prelude            hiding (Line)
import           Fmt


class Optional f a where
    (???) :: f a -> a -> a
    infixr 7 ???

instance Optional Maybe a where
    Just x ??? _  = x
    Nothing ??? x = x


