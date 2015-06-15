{-# LANGUAGE Rank2Types, ImpredicativeTypes #-}

module Sinner 
--(
--    C, mkNote, mkSinusoide, (.+), (.-), (.*), (./), 
--    mkNoise, mkWhiteNoise, 
--    mkAmplitudeModulator, setAmplitudeModulation, setADSR, setClipping, setDistortion)
where

import Data.List
import System.Random
import Math.FFT
import Math.FFT.Base (FFTWReal)
import Data.Complex (Complex(..), realPart)
import Data.Vector.Storable (unsafeToForeignPtr, fromList)
import Data.Array.CArray.Base
import Foreign.Marshal.Array (peekArray)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable

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

baseFrequency :: (Floating a, Ord a) => Note -> a
baseFrequency C = 261.23
baseFrequency CSharp = 277.18
baseFrequency DFlat = baseFrequency CSharp
baseFrequency D = 293.66
baseFrequency DSharp = 311.13
baseFrequency EFlat = baseFrequency DSharp
baseFrequency E = 329.63
baseFrequency ESharp = baseFrequency F
baseFrequency FFlat = baseFrequency E
baseFrequency F = 349.23
baseFrequency FSharp = 369.99
baseFrequency GFlat = baseFrequency FSharp
baseFrequency G = 392.00
baseFrequency GSharp = 415.30
baseFrequency AFlat = baseFrequency GSharp
baseFrequency A = 440
baseFrequency ASharp = 466.16
baseFrequency BFlat = baseFrequency ASharp
baseFrequency B = 493.88
baseFrequency CFlat = baseFrequency B

frequencyOnOctave :: (Floating a, Ord a) => a -> Integer -> a
frequencyOnOctave base octave
    | freq < 20 = frequencyOnOctave base (octave+1)
    | freq > 20000 = frequencyOnOctave base (octave-1)
    | otherwise = freq
    where freq = base * (2**((fromInteger octave) - 3))

mkNote :: (Floating a, Ord a) => Note -> Integer -> a
mkNote n = frequencyOnOctave (baseFrequency n)

-- Sinusoides --

--data WaveForm
--    = Sine
--    | Square
--    | Triangle
--    | Sawtooth

--data Signal = Signal {
--    frequency :: Double,
--    amplitude :: Double,
--    phase :: Double,
--    waveform :: WaveForm
--}

mkSinusoide :: (Floating a, Enum a) => (a -> a) -> (a -> a) -> a -> [a]
mkSinusoide func freq duration = [sin $ 2 * pi * (freq (t*(1/duration))) * (func t) | t <- [0, (1/44100)..duration]]

zipWithDefault :: (Num a, Num b, Num c) 
    => a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithDefault da db f la lb = take len $ zipWith f la' lb' 
                        where
                            len = max (length la) (length lb)
                            la' = la ++ (repeat da)
                            lb' = lb ++ (repeat db)

(.+) :: Floating a => [a] -> [a] -> [a]
f1 .+ f2 = zipWithDefault 0 0 (+) f1 f2

(.-) :: Floating a => [a] -> [a] -> [a]
f1 .- f2 = zipWithDefault 0 0 (-) f1 f2

(.*) :: Floating a => [a] -> [a] -> [a]
f1 .* f2 = zipWithDefault 1 1 (*) f1 f2

(./) :: (Floating a, Eq a) => [a] -> [a] -> [a]
f1 ./ f2 = zipWithDefault 1 1 (\x y -> if y == 0 then 0 else x / y) f1 f2

-- Noise --
unsafeListToCArray xs =
    let (ptr, beg, end) = (unsafeToForeignPtr . fromList) xs
    in unsafeForeignPtrToCArray ptr (beg, end)

unsafeCArrayToList xs =
    let (size, ptr) = toForeignPtr xs
    in withForeignPtr ptr (peekArray size)

mkNoise :: (Enum a, Floating a, Storable a, RealFrac a, FFTWReal a) => (a -> a) -> a -> IO [a]
mkNoise func duration = do
    before <- unsafeListToCArray $ map (\i -> func i :+ 0) [0..(duration * 44100.0)]
    after <- unsafeCArrayToList $ idft before
    return $ map realPart after

mkWhiteNoise :: (Enum a, Floating a, RealFrac a, FFTWReal a) => a -> IO [a]
mkWhiteNoise = mkNoise (\x -> 50)

-- Amplitude Modulator --

data AmplitudeModulator = AmplitudeModulator {
    amFunction :: forall a. Floating a => (a -> a),
    amStart :: forall a. (Num a, Floating a) => a,
    amEnd :: forall a. (Num a, Floating a) => a
}

mkAmplitudeModulator :: [((forall a. Floating a => a -> a), 
                        (forall a. (Num a, Floating a) => a),
                        (forall a. (Num a, Floating a) => a))] -> [AmplitudeModulator]
mkAmplitudeModulator = map (\(f, s, e) -> AmplitudeModulator f s e)

setAmplitudeModulation :: (Ord a, Enum a, Floating a) => [AmplitudeModulator] -> [a] -> [a]
setAmplitudeModulation [] sine = sine
setAmplitudeModulation (x:xs) sine = setAmplitudeModulation xs (zipWith applyAM sine [0..])
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

setADSR :: (Enum a, Ord a, Floating a) => [AmplitudeModulator] -> [a] -> [a]
setADSR xs 
    | (sum $ map (\x -> amEnd x - amStart x) xs) /= 1 = error $ "Wrong ADSR"
    | otherwise = setAmplitudeModulation xs


setClipping :: (Ord a, Floating a) => a -> [a] -> [a]
setClipping threshold = map (\x -> if x > threshold || x < threshold*(-1) then 0 else x)

setDistortion :: (Ord a, Floating a) => a -> [a] -> [a]
setDistortion = setClipping

-- Filters --