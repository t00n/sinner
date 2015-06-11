{-# LANGUAGE Rank2Types #-}

module Sinner (
    Note, note, mkSinusoide, (.+), (.-), (.*), (./), 
    mkNoise, mkWhiteNoise,
    AmplitudeModulator, amplitudeModulation, adsr, clipping, distortion)
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
    amFunction :: forall a. (Floating a) => (a -> a),
    amStart :: forall a. (Num a, Floating a) => a,
    amEnd :: forall a. (Num a, Floating a) => a
}

amplitudeModulation :: (Ord a, Enum a, Floating a) => [AmplitudeModulator] -> [a] -> [a]
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

adsr :: (Enum a, Ord a, Floating a) => [AmplitudeModulator] -> [a] -> [a]
adsr adsrVars 
    | (sum $ map (\x -> amEnd x - amStart x) adsrVars) /= 1 = error $ "Wrong ADSR"
    | otherwise = amplitudeModulation adsrVars


clipping :: (Ord a, Floating a) => a -> [a] -> [a]
clipping threshold = map (\x -> if x > threshold || x < threshold*(-1) then 0 else x)

distortion :: (Ord a, Floating a) => a -> [a] -> [a]
distortion = clipping

-- Filters --