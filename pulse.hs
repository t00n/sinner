{-# LANGUAGE MultiParamTypeClasses #-}
module Pulse where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Sound.Pulse.Simple

import Sinner

data PulseState = PulseState {
    sampling :: Float,
    simple :: Simple
}

play :: Sinus -> Float -> StateT PulseState IO ()
play fsin duration = do 
    state <- get
    let si = simple state
    let sa = sampling state
    lift $ simpleWrite si ([fsin t | t <- [0..sa * duration]] :: [Float]) 
