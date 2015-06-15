
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
        let start = setDistortion (0.8 :: Double) $ mkSinusoide (\x -> x) (\t -> (mkNote C 3) - ((mkNote C 3) -(mkNote C 1))*t*t) 0.2
        let other = (mkSinusoide id (\t -> mkNote C 2) 0.4)
        let sinusoide = start .+ other
        let afterMod = setAmplitudeModulation (mkAmplitudeModulator [(\x -> 1.0 - x/2, 0, 0.1), (\x -> 1.0 - x/4, 0.1, 0.2), (\x -> 0.8, 0.2, 0.5), (\x -> 0.5 - x/2, 0.8, 1.0)]) sinusoide
        let afterDistortion = afterMod
        play afterDistortion
        liftIO $ plot afterDistortion
        --noise <- liftIO $ mkWhiteNoise 1.0
        --play noise
        --liftIO $ plot noise
        --liftIO $ plot $ mkWhiteNoise 1
        )
    simpleDrain simple
    simpleFree simple