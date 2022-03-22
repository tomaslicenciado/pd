module Cartas
(
    Carta,
    Palo(..),
    carta,
    mazo,
    as,
    rey,
    sota,
    caballo,
    macho,
    hembra,
    anchoCopa,
    anchoOro,
    empate,
    orden,
    mismoPalo,
    puntos,
    nada
) where

import Data.List ( elemIndex )
data Palo = Oros | Espadas | Bastos | Copas | Cualquier deriving (Show)

instance Eq Palo where
    (==) Cualquier _ = True 
    (==) _ Cualquier = True 
    (==) Oros Oros = True
    (==) Espadas Espadas = True
    (==) Bastos Bastos = True
    (==) Copas Copas = True
    (==) _ _ = False
    

as :: Int
as = 1

sota :: Int
sota = 10

caballo :: Int
caballo = 11

rey :: Int
rey = 12

data Carta = C { numero :: Int, palo :: Palo}

instance Ord Carta where
    compare c1 c2 
        | palo c1 == palo c2 = compare (numero c1) (numero c2) 
        | otherwise = compare (elemIndex c2 orden) (elemIndex c1 orden)

instance Show Carta where
    show c
        | c == macho = "Macho"
        | c == hembra = "Hembra"
        | c == anchoCopa = "Ancho de Copas"
        | c == anchoOro = "Ancho de Oros"
        | numero c == as = "As de " ++ show (palo c)
        | numero c == sota = "Sota de " ++ show (palo c)
        | numero c == caballo = "Caballo de " ++ show (palo c)
        | numero c == rey = "Rey de " ++ show (palo c)
        | otherwise = show (numero c) ++ " de " ++ show (palo c)

instance Eq Carta where
    (==) c1 c2 
        | numero c1 == numero c2 = palo c1 == palo c2
        | otherwise = False

carta :: Int -> Palo -> Carta
carta v p
        | v `elem` [as..7]++[sota..rey] = C v p
        | otherwise = C 0 p

mazo ::[Carta]
mazo = [C v p | v <- [as..7]++[sota..rey], p <- [Oros, Espadas, Bastos, Copas]]

macho :: Carta
macho = C as Espadas

hembra :: Carta
hembra = C as Bastos

anchoOro :: Carta
anchoOro = C as Oros

anchoCopa :: Carta
anchoCopa = C as Copas

empate :: Carta
empate = C 0 Cualquier

nada :: Carta
nada = C 0 Cualquier

puntos :: Carta -> Int
puntos c
        | numero c < sota = numero c
        | otherwise = 0

paloIgual :: Carta -> Carta -> Bool
paloIgual c1 c2 = palo c1 == palo c2

mismoPalo :: [Carta] -> [[Carta]]
mismoPalo [] = []
mismoPalo [x] = [[x]]
mismoPalo xs = (head xs : takeWhile (paloIgual (head xs)) (tail xs)): mismoPalo (dropWhile (paloIgual (head xs)) (tail xs))

orden :: [Carta]
orden = [macho, hembra, C 7 Espadas, C 7 Oros, C 3 Cualquier , C 2 Cualquier , C 1 Cualquier ,
        C rey Cualquier, C caballo Cualquier, C 7 Cualquier, C 6 Cualquier, C 5 Cualquier, C 4 Cualquier]