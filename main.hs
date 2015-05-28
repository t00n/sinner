
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
        let sinusoide = (mkSinusoide id (note A 2) 1)
        let afterMod = amplitudeModulation [AmplitudeModulator (\x -> 0.5) 0 0.25, AmplitudeModulator (\x -> 2) 0.25 0.5] sinusoide
        --play afterMod
        liftIO $ plot sinusoide
        liftIO $ plot afterMod
        )
    simpleDrain simple
    simpleFree simple