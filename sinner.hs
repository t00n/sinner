import Sound.Pulse.Simple

infixl 6 :+:, :-:
infixl 7 :*:, :/:
infixl 8 :**:


type Period = Float
type Amplitude = Float

data Func
    = Var
    | Lit Float
    | Func :+: Func
    | Func :-: Func
    | Func :*: Func
    | Func :/: Func
    | Func :**: Func
    deriving (Show)

data Sinus 
    = Sinus Func
    deriving (Show)

evalFunc :: Func -> [Float]
evalFunc Var = [i / 10 | i <- [0..10]]
evalFunc (Lit n) = [n | i <- [0..10]]
evalFunc (a :+: b) = zipWith (+) (evalFunc a) (evalFunc b)
evalFunc (a :-: b) = zipWith (-) (evalFunc a) (evalFunc b)
evalFunc (a :*: b) = zipWith (*) (evalFunc a) (evalFunc b)
evalFunc (a :/: b) = zipWith (/) (evalFunc a) (evalFunc b)
evalFunc (a :**: b) = zipWith (**) (evalFunc a) (evalFunc b)

eval :: Sinus -> [Float]
eval (Sinus x) = map sin (evalFunc x)

sinus :: (Float -> Float) -> Float -> (Float -> Float)
sinus func freq = \t -> sin $ 2 * pi * freq * ((func t) / 44100)

exec :: Simple -> (Float -> Float) -> Float -> IO ()
exec s f duration = do
    simpleWrite s ([f t | t<-[0..44100*duration]] :: [Float])

main :: IO ()
main = do
    s<-simpleNew Nothing "Sinner" Play Nothing "This is Sinner" (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing
    --exec s (sinus id) 1
    exec s (sinus (\x -> x*log(x)) 440) 5
    simpleDrain s
    simpleFree s