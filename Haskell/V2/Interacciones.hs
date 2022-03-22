{-# LANGUAGE MultiWayIf #-}
module Interacciones
(
    Canto(..),
    mezclarMazo,
    repartir,
    puntosEnvidoMano,
    ganaCarta,
    cola,
    mejorCarta
)where

import Mazo ( carta, mazo, nada, orden, valorPEnvido, Carta(palo), Palo(..))
import Data.List (sortBy, elemIndex, sort, transpose)
import Data.Time.Clock ( UTCTime(utctDayTime), getCurrentTime, DiffTime )
import Data.Functor ( (<&>) )
import System.IO.Unsafe (unsafePerformIO)
import Data.Bifunctor (bimap)


-----------------------------------------------------------------------------------------------------------------------
-- Cantos
data Canto = TRUCO | RETRUCO | VALECUATRO | ENVIDO
    | ENVIDOENVIDO | REALENVIDO | FALTAENVIDO | SILENCIO | QUIERO | NOQUIERO deriving (Eq)

instance Show Canto where
    show c
        | c == TRUCO = "Truco"
        | c == RETRUCO = "Re Truco"
        | c == VALECUATRO = "Vale Cuatro"
        | c == ENVIDO = "Envido"
        | c == ENVIDOENVIDO = "Envido Envido"
        | c == REALENVIDO = "Real Envido"
        | c == FALTAENVIDO = "Falta Envido"
        | c == QUIERO = "Quiero"
        | c == NOQUIERO = "No quiero"
        | otherwise = " "
-----------------------------------------------------------------------------------------------------------------------

-- Definir si dos cartas son del mismo palo
mismoPalo :: Carta -> Carta -> Bool
mismoPalo c1 c2 = palo c1 == palo c2

-- Agrupar un conjunto de cartas por palo
agruparPorPalo :: [Carta] -> [[Carta]]
agruparPorPalo [] = []
agruparPorPalo [x] = [[x]]
agruparPorPalo xs = (head xs : takeWhile (mismoPalo (head xs)) (tail xs)) : agruparPorPalo (dropWhile (mismoPalo (head xs)) (tail xs))

----------------------------------------------------------------------------------
-- Calcular puntos para el envido en la mano
-- Suponiendo un conjunto de cartas todas del mismo palo, suma el valor de las 2 cartas de mayor
-- puntaje para envido (0 las "negras" y su número el resto)y les suma 20
sumaPuntosEnvido :: [Carta] -> Int
sumaPuntosEnvido [] = 0
sumaPuntosEnvido xs
    | length xs >= 3 = sumaPuntosEnvido (filter ((minimum  (map valorPEnvido xs) <). valorPEnvido) xs)
    | otherwise = sum (map valorPEnvido xs) + 20

-- Calcula el puntaje máximo para envido dentro de un conjunto de cartas de mismo o diferente palo
puntosEnvidoMano :: [Carta] -> Int
puntosEnvidoMano [] = 0
puntosEnvidoMano xs = maximum $ map sumaPuntosEnvido (agruparPorPalo xs)
-----------------------------------------------------------------------------------

-- Que carta gana en una mano de truco. Nada significa tablas o empate
ganaCarta :: Carta -> Carta -> Carta
ganaCarta c1 c2
    | elemIndex c1 orden > elemIndex c2 orden = c2
    | elemIndex c1 orden < elemIndex c2 orden = c1
    | otherwise = nada

-----------------------------------------------------------------------------------
-- Función para obtener el tiempo actual en segundos
{-# NOINLINE time #-}
time :: IO DiffTime
time = getCurrentTime Data.Functor.<&> utctDayTime

-- Semilla para posición aleatoria a la cual se le pasa el tiempo obtenido en time
seed :: IO DiffTime -> Int
seed t = do
    let t1 = unsafePerformIO t
    (floor . toRational) t1

-- "Redefinición de tail" para saltearse el error de lista vacía para la cual la función tail
-- no está definida
cola :: [a] -> [a]
cola [] = []
cola xs = tail xs

-- Genera un grupo aleatorio de cartas DISTINTAS tomadas desde una lista
cartasAleatorias :: Int -> [Carta] -> [Carta]
cartasAleatorias 0 _ = []
cartasAleatorias _ [] = []
cartasAleatorias n xs = do
    let pos = mod (seed time) (length xs)
        cabeza = xs !! pos
    cabeza : cartasAleatorias (n-1) (takeWhile (/= cabeza) xs ++ cola (dropWhile (/= cabeza) xs))

-- El mazo tiene 40 cartas. Si tomo 40 cartas distintas aleatoriamente del mazo estoy reordenando el mazo
-- con orden aleatorio. Ésto es equivalente a "mezclar el mazo de cartas"
mezclarMazo :: [Carta]
mezclarMazo = cartasAleatorias 40 mazo

-- Repartir simula la acción "real" que se da al repartir cartas en una partida de truco
repartir :: ([Carta], [Carta])
repartir = do
    let m = mezclarMazo
        c = transpose $ [take 2 m] ++ [(take 2 . drop 2) m] ++ [(take 2 . drop 4) m]
    (head c, last c)

-- Dicernir la mejor carta a jugar de las disponibles
mejorCarta :: [Carta] -> ([Carta],[Carta]) -> Carta
mejorCarta [] _ = nada
mejorCarta cts (js1,js2)
    | length js1 + length js2 == 0 = if minimum cts == maximum cts then minimum cts else (minimum . init . sort) js1
    | length js1 + length js2 == 1 = if
        | ganaCarta ((head . tail . sort) cts) (head js2) `elem` cts -> (head . tail . sort) cts
        | (ganaCarta (maximum cts) (head js2) `elem` cts) && (maximum cts >= carta 3 Cualquier) -> if
            ganaCarta ((head . tail . sort) cts) (head js2) == nada then (head . tail . sort) cts
            else maximum cts
        | otherwise -> minimum cts
    | length js1 + length js2 < 4 = if ganaCarta (head js1) (head js2) `elem` js1
        then minimum cts
        else if ganaCarta (maximum cts) (head js2) `elem` cts
            then maximum cts
            else nada
    | length js1 + length js2 < 6 = if ganaCarta (head cts) (head js2) `elem` cts then head cts else nada
    | otherwise = nada
