
import Control.Monad.Trans.State

import Sound.Pulse.Simple
import Pulse
import Sinus

main :: IO ()
main = do
    s<-simpleNew Nothing "Sinner" Play Nothing "This is Sinner" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    runStateT (tellPulse (sinus (\x -> x) 440) 1) (PulseState 44100 s)
    simpleDrain s
    simpleFree s