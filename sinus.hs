module Sinus where

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
