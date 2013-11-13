module Util where

liftFst f (x,y) = (f x, y)
liftSnd f (x,y) = (x, f y)
