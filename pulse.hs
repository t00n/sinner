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

play :: Sinus -> StateT PulseState IO ()
play fsin = do 
    state <- get
    let si = simple state
    let sa = sampling state
    lift $ simpleWrite si fsin
