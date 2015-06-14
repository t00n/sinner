
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Sound.Pulse.Simple
import Graphics.Gnuplot.Simple

import Pulse
import Sinner

plot :: [Double] -> IO ()
plot xs = do
    plotList [] (zip [0..] xs :: [(Double, Double)])

main :: IO ()
main = do
    simple <- simpleNew Nothing "Sinner" Play Nothing "This is Sinner" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    let state = PulseState 44100 simple
    flip runStateT state (do 
        --let start = distortion (0.8 :: Double) $ mkSinusoide (\x -> x) (\t -> (note C 3) - ((note C 3) -(note C 1))*t*t) 0.2
        --let other = (mkSinusoide id (\t -> note C 2) 0.4)
        --let sinusoide = start .+ other
        --let afterMod = amplitudeModulation [AmplitudeModulator (\x -> 1.0 - x/2) 0 0.1,AmplitudeModulator (\x -> 1.0 - x/4) 0.1 0.2,AmplitudeModulator (\x -> 0.8) 0.2 0.5, AmplitudeModulator (\x -> 0.5 - x/2) 0.8 1.0] sinusoide
        --let afterDistortion = afterMod
        --play sinusoide
        --play afterMod
        --play $ mkSinusoide id (\t -> 0) 0.1
        --play afterDistortion
        --liftIO $ plot sinusoide
        --liftIO $ plot afterMod
        --liftIO $ plot afterDistortion
        --noise <- liftIO $ mkWhiteNoise 1.0
        --play noise
        --liftIO $ plot noise
        --liftIO $ plot $ mkWhiteNoise 1
        )
    simpleDrain simple
    simpleFree simple