
import Control.Monad.Trans.State

import Sound.Pulse.Simple

import Pulse
import Sinner

main :: IO ()
main = do
    simple <- simpleNew Nothing "Sinner" Play Nothing "This is Sinner" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    let state = PulseState 44100 simple
    flip runStateT state (do 
        play ((sinus id (note A 2)) .* (sinus id 440) .* (sinus id 110) .* (sinus (\x -> x*5) 55)) 1
        )
    simpleDrain simple
    simpleFree simple