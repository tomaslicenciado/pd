module Mazo
(
    Carta(..),
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
    nada,
    orden,
    paloIgual,
    valorPEnvido
) where

import Data.List( elemIndex )

-- Definimos los palos
data Palo = Oros | Espadas | Basto | Copas | Cualquier deriving ( Show )

-- Definimos la igualdad de los palos
instance Eq Palo where
    ( == ) Cualquier _ = True
    ( == ) _ Cualquier = True
    ( == ) Oros Oros = True 
    ( == ) Espadas Espadas = True
    ( == ) Basto Basto = True 
    ( == ) Copas Copas = True
    ( == ) _ _ = False

---------------------------------------------------------
-- Definimos los nombres comunes para las cartas con cara
as :: Int
as = 1

sota :: Int
sota = 10

caballo :: Int
caballo = 11

rey :: Int
rey = 12
---------------------------------------------------------

-- Definimos lo que es una carta
data Carta = C { numero :: Int, palo :: Palo}

------------------------------------------------------------------------------------------------------------
-- Cuando una carta es mayor que otra
instance Ord Carta where
    compare c1 c2 = compare (elemIndex c2 orden) (elemIndex c1 orden)

-- Cuando una carta es igual a otra 
instance Eq Carta where
    (==) c1 c2 
        | numero c1 == numero c2 = paloIgual c1 c2
        | otherwise = False
-------------------------------------------------------------------------------------------------------------

-- Definimos cómo se imprime una carta por pantalla
instance Show Carta where
    show c
        | c == macho = "Macho"
        | c == hembra = "Hembra"
        | c == anchoCopa = "Ancho de Copas"
        | c == anchoOro = "Ancho de Oros"
        | c == nada = ""
        | numero c == as = "As de " ++ show (palo c)
        | numero c == sota = "Sota de " ++ show (palo c)
        | numero c == caballo = "Caballo de " ++ show (palo c)
        | numero c == rey = "Rey de " ++ show (palo c)
        | otherwise = show (numero c) ++ " de " ++ show (palo c)

--------------------------------------------------------
-- Definimos los nombres de las cartas importantes
macho :: Carta
macho = C as Espadas

hembra :: Carta
hembra = C as Basto

anchoOro :: Carta
anchoOro = C as Oros

anchoCopa :: Carta
anchoCopa = C as Copas
--------------------------------------------------------

-- Definimos el orden específico de importancia para las cartas en el truco, ésto facilita la comparación
orden :: [Carta]
orden = [macho, hembra, C 7 Espadas, C 7 Oros, C 3 Cualquier , C 2 Cualquier , C 1 Cualquier ,
        C rey Cualquier, C caballo Cualquier, C 7 Cualquier, C 6 Cualquier, C 5 Cualquier, C 4 Cualquier, nada]

nada :: Carta
nada = C 0 Cualquier

-- Listamos TODAS las cartas que se usan en el truco
mazo :: [Carta]
mazo = [ C v p | v <- [as..7]++[sota..rey], p <- [Oros, Espadas, Basto, Copas]]

-- Construir una carta en base a un número y un palo. Validamos contra las cartas del mazo
carta :: Int -> Palo -> Carta
carta v p
    | v `elem` [as..7]++[sota..rey] = C v p
    | otherwise = nada

-- Cuánto vale cada carta al momento de sumar su valor para el canto de envido
valorPEnvido :: Carta -> Int
valorPEnvido c
    | numero c < sota = numero c
    | otherwise = 0

-- Comparar si dos cartas son del mismo palo. (Para simplificar notación)
paloIgual :: Carta -> Carta -> Bool 
paloIgual c1 c2 = palo c1 == palo c2
