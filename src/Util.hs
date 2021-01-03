
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Util where



class Optional f a where
    (???) :: f a -> a -> a
    infixr 7 ???

instance Optional Maybe a where
    Just x ??? _  = x
    Nothing ??? x = x

