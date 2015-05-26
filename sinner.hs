import Sound.Pulse.Simple
import Prelude hiding ((+), (-), (/))

import Sinus

exec :: Simple -> Sinus -> Float -> IO ()
exec s f duration = do
    simpleWrite s ([f t | t <- [0..44100 Prelude.* duration]] :: [Float])

main :: IO ()
main = do
    s<-simpleNew Nothing "Sinner" Play Nothing "This is Sinner" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    --exec s (sinus id) 1
    exec s ((sinus (\x -> x) 440) Sinus.* (sinus (\x -> x) 440)) 2
    simpleDrain s
    simpleFree s