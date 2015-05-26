import Sound.Pulse.Simple

--infixl 6 :+:, :-:
--infixl 7 :*:, :/:
--infixl 8 :**:
--infixl 9 :.:


--type Period = Float
--type Amplitude = Float

--data Func
--    = Var
--    | Lit Float
--    | Func :+: Func
--    | Func :-: Func
--    | Func :*: Func
--    | Func :/: Func
--    | Func :**: Func
--    | Func :.: Func
--    deriving (Show)

--data Sinus 
--    = Sinus Func
--    deriving (Show)

--eval :: Func -> (Float -> Float)
--eval Var = \x -> x
--eval (Lit n) = \x -> n
--eval (a :+: b) = \x -> ((eval a) x) + ((eval b) x)
--eval (a :-: b) = \x -> ((eval a) x) - ((eval b) x)
--eval (a :*: b) = \x -> ((eval a) x) * ((eval b) x)
--eval (a :/: b) = \x -> ((eval a) x) / ((eval b) x)
--eval (a :**: b) = \x -> ((eval a) x) ** ((eval b) x)
--eval (a :.: b) = \x -> ((eval a) . (eval b)) x

type Sinus = (Float -> Float)

sinus :: Sinus -> Float -> Sinus
sinus func freq = \t -> sin $ 2 * pi * freq * ((func t) / 44100)

sinusAdd :: Sinus -> Sinus -> Sinus
sinusAdd f1 f2 = \t -> (f1 t) + (f2 t)

sinusSubtract :: Sinus -> Sinus -> Sinus
sinusSubtract f1 f2 = \t -> (f1 t) - (f2 t)

sinusMultiply :: Sinus -> Sinus -> Sinus
sinusMultiply f1 f2 = \t -> (f1 t) * (f2 t)

sinusDivide :: Sinus -> Sinus -> Sinus
sinusDivide f1 f2 = \t -> if (f2 t) == 0 then 0 else (f1 t) / (f2 t)

exec :: Simple -> Sinus -> Float -> IO ()
exec s f duration = do
    simpleWrite s ([f t | t<-[0..44100*duration]] :: [Float])

main :: IO ()
main = do
    s<-simpleNew Nothing "Sinner" Play Nothing "This is Sinner" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    --exec s (sinus id) 1
    exec s (sinusDivide (sinus (\x -> x) 440) (sinus(\x -> x) 440)) 2
    simpleDrain s
    simpleFree s