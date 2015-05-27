module Sinner where

import Data.List
import Data.Maybe

type Sinus = (Float -> Float)

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

sinus :: Sinus -> Float -> Sinus
sinus func freq = \t -> sin $ 2 * pi * freq * ((func t) / 44100)

(.+) :: Sinus -> Sinus -> Sinus
f1 .+ f2 = \t -> (f1 t) + (f2 t)

(.-) :: Sinus -> Sinus -> Sinus
f1 .- f2 = \t -> (f1 t) - (f2 t)

(.*) :: Sinus -> Sinus -> Sinus
f1 .* f2 = \t -> (f1 t) * (f2 t)

(./) :: Sinus -> Sinus -> Sinus
f1 ./ f2 = \t -> if (f2 t) == 0 then 0 else (f1 t) / (f2 t)

computeFreq :: (Floating a, Ord a) => a -> Integer -> a
computeFreq base octave
    | freq < 20 = computeFreq base (octave+1)
    | freq > 20000 = computeFreq base (octave-1)
    | otherwise = freq
    where freq = base * (2**((fromInteger octave) - 3))

note :: (Floating a, Ord a) => Note -> Integer -> a
note n = computeFreq (noteFreq n)
