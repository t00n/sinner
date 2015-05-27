module Pulse where

import Sound.Pulse.Simple
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Sinus

data PulseState = PulseState {
    sampling :: Float,
    simple :: Simple
}

tellPulse :: Sinus -> Float -> StateT PulseState IO ()
tellPulse fsin duration = do 
    state <- get
    let s = simple state
    lift $ simpleWrite s ([fsin t | t <- [0..44100 Prelude.* duration]] :: [Float])
