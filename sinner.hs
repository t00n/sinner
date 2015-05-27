
import Control.Monad.Trans.State

import Sound.Pulse.Simple
import Pulse
import Sinus

program :: StateT PulseState IO ()
program = do 
    tellPulse (sinus (\x -> x) 440) 1
    tellPulse (sinus (\x -> x) 880) 1

main :: IO ()
main = do
    simple <- simpleNew Nothing "Sinner" Play Nothing "This is Sinner" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    let state = PulseState 44100 simple
    runStateT program state
    simpleDrain simple
    simpleFree simple