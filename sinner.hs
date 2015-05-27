module Sinner where

type Sinus = (Float -> Float)

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

computeFreq :: Floating a => a -> Integer -> a
computeFreq base octave = base * (2**((fromInteger octave) - 3))

a :: Floating a => Integer -> a
a = computeFreq 440

b :: Floating a => Integer -> a
b = computeFreq 493.88