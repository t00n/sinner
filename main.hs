
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Sound.Pulse.Simple
import Graphics.Gnuplot.Simple

import Pulse
import Sinner

plot :: [Float] -> IO ()
plot xs = do
    plotList [] ((zip [0..] xs) :: [(Float, Float)])

main :: IO ()
main = do
    simple <- simpleNew Nothing "Sinner" Play Nothing "This is Sinner" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    let state = PulseState 44100 simple
    flip runStateT state (do 
        let sinusoide = (mkSinusoide id (note C 2) 0.3) .+ (mkSinusoide id (note C 3) 0.3)
        let afterMod = amplitudeModulation [AmplitudeModulator (\x -> x) 0 0.2,AmplitudeModulator (\x -> 1.0 - x/5) 0.2 0.3,AmplitudeModulator (\x -> 0.8) 0.2 0.8, AmplitudeModulator (\x -> 0.8 - x*4/5) 0.8 1.0] sinusoide
        let afterDistortion = distortion 0.8 afterMod
        play afterDistortion
        --liftIO $ plot sinusoide
        liftIO $ plot afterMod
        liftIO $ plot afterDistortion
        )
    simpleDrain simple
    simpleFree simple