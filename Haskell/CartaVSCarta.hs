module CartaVSCarta(
    repartir,
    ganaCarta,
    puntosEnvido
)where
import Cartas
import Data.Char ( ord )
import Data.List ( elemIndex, sort )

posFromSeed :: [Char] -> Int -> Int
posFromSeed seed = mod (sum $ map ord seed)

repartir :: Int -> String -> [Carta] -> [Carta]
repartir 0 _  _ = []
repartir n seed cs = do
    let pos = posFromSeed seed (length cs)
    cs !! pos : repartir (n-1) seed [x | x <- cs, x /= (cs !! pos)]

ganaCarta :: Carta -> Carta -> Carta
ganaCarta c1 c2 
    | elemIndex c1 orden > elemIndex c2 orden = c2
    | elemIndex c1 orden < elemIndex c2 orden = c1
    | otherwise = empate 

pEnvido :: [Int] -> Int
pEnvido [] = 0
pEnvido xs
    | length xs == 3 = sum (drop 1 (sort xs)) + 20
    | length xs == 2 = sum xs + 20
    | otherwise = sum xs + 10

puntosEnvido :: [Carta] -> Int
puntosEnvido [] = 0
puntosEnvido xs = do
    let gruposXPalos = mismoPalo xs
        numerosXPalos = map (map puntos) gruposXPalos
    maximum (map pEnvido numerosXPalos)