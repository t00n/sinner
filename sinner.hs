module Sinner where

import Data.List

-- Notes --
data Note
    = C
    | CSharp
    | DFlat
    | D
    | DSharp
    | EFlat
    | E
    | ESharp
    | FFlat
    | F
    | FSharp
    | GFlat
    | G
    | GSharp
    | AFlat
    | A
    | ASharp
    | BFlat
    | B
    | CFlat
    deriving (Show, Ord, Eq)

noteFreq :: (Floating a, Ord a) => Note -> a
noteFreq C = 261.23
noteFreq CSharp = 277.18
noteFreq DFlat = noteFreq CSharp
noteFreq D = 293.66
noteFreq DSharp = 311.13
noteFreq EFlat = noteFreq DSharp
noteFreq E = 329.63
noteFreq ESharp = noteFreq F
noteFreq FFlat = noteFreq E
noteFreq F = 349.23
noteFreq FSharp = 369.99
noteFreq GFlat = noteFreq FSharp
noteFreq G = 392.00
noteFreq GSharp = 415.30
noteFreq AFlat = noteFreq GSharp
noteFreq A = 440
noteFreq ASharp = 466.16
noteFreq BFlat = noteFreq ASharp
noteFreq B = 493.88
noteFreq CFlat = noteFreq B

computeFreq :: (Floating a, Ord a) => a -> Integer -> a
computeFreq base octave
    | freq < 20 = computeFreq base (octave+1)
    | freq > 20000 = computeFreq base (octave-1)
    | otherwise = freq
    where freq = base * (2**((fromInteger octave) - 3))

note :: (Floating a, Ord a) => Note -> Integer -> a
note n = computeFreq (noteFreq n)

-- Sinusoides --

--data WaveForm
--    = Sine
--    | Square
--    | Triangle
--    | Sawtooth

--data Signal = Signal {
--    frequency :: Float,
--    amplitude :: Float,
--    phase :: Float,
--    waveform :: WaveForm
--}

type Sinusoide = [Float]

mkSinusoide :: (Float -> Float) -> Float -> Float -> Sinusoide
mkSinusoide func freq duration = [sin $ 2 * pi * freq * ((func t) / 44100) | t <- [0..duration * 44100]]

zipWithDefault :: (Num a, Num b, Num c) 
    => a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault da db f la lb = take len $ zipWith f la' lb' 
                        where
                            len = max (length la) (length lb)
                            la' = la ++ (repeat da)
                            lb' = lb ++ (repeat db)

(.+) :: Sinusoide -> Sinusoide -> Sinusoide
f1 .+ f2 = zipWithDefault 0 0 (+) f1 f2

(.-) :: Sinusoide -> Sinusoide -> Sinusoide
f1 .- f2 = zipWithDefault 0 0 (-) f1 f2

(.*) :: Sinusoide -> Sinusoide -> Sinusoide
f1 .* f2 = zipWithDefault 1 1 (*) f1 f2

(./) :: Sinusoide -> Sinusoide -> Sinusoide
f1 ./ f2 = zipWithDefault 1 1 (\x y -> if y == 0 then 0 else x / y) f1 f2

-- Amplitude Modulator --

data AmplitudeModulator = AmplitudeModulator {
    amFunction :: (Float -> Float),
    amStart :: Float,
    amEnd :: Float
}

amplitudeModulation :: [AmplitudeModulator] -> Sinusoide -> Sinusoide
amplitudeModulation [] sine = sine
amplitudeModulation (x:xs) sine = amplitudeModulation xs (zipWith applyAM sine [0..])
    where
        f = amFunction x
        start = amStart x
        end = amEnd x
        len = length sine
        applyAM x i
            | start <= i/lenfloat && i/lenfloat < end = x * (f (offset / interval))
            | otherwise = x
            where
                lenfloat = fromIntegral len
                interval = (end - start) * lenfloat
                offset = i - start * lenfloat

adsr :: [AmplitudeModulator] -> Sinusoide -> Sinusoide
adsr adsrVars sine 
    | (sum $ map (\x -> amEnd x - amStart x) adsrVars) /= 1 = error $ "Wrong ADSR"
    | otherwise = amplitudeModulation adsrVars sine


distortion :: Float -> Sinusoide -> Sinusoide
distortion threshold sine = map (\x -> if x > threshold || x < threshold*(-1) then 0 else x) sine