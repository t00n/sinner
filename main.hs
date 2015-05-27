
import Control.Monad.Trans.State

import Sound.Pulse.Simple

import Pulse
import Sinner

main :: IO ()
main = do
    simple <- simpleNew Nothing "Sinner" Play Nothing "This is Sinner" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    let state = PulseState 44100 simple
    flip runStateT state (do 
        play ((sinus id (note A 2) 1) .+ (sinus id (note A 2) 2) .* (sinus id (note A 1) 1))
        )
    simpleDrain simple
    simpleFree simple