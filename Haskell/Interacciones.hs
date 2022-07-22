{-# LANGUAGE MultiWayIf #-}
module Interacciones
(
    Canto(..),
    mezclarMazo,
    repartir,
    puntosEnvidoMano,
    ganaCarta,
    cola,
    opciones
)where

import Mazo ( carta, mazo, nada, orden, valorPEnvido, Carta(palo), Palo(..), paloIgual)
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

respuestasEnvido :: [String]
respuestasEnvido = map show [ENVIDO, ENVIDOENVIDO, REALENVIDO, FALTAENVIDO, QUIERO, NOQUIERO]
-----------------------------------------------------------------------------------------------------------------------

-- Agrupar un conjunto de cartas por palo
agruparPorPalo :: [Carta] -> [[Carta]]
agruparPorPalo [] = []
agruparPorPalo [x] = [[x]]
agruparPorPalo xs = filter (paloIgual (head xs)) xs : agruparPorPalo (filter (not . paloIgual (head xs)) xs)

----------------------------------------------------------------------------------
-- Calcular puntos para el envido en la mano
-- Suponiendo un conjunto de cartas todas del mismo palo, suma el valor de las 2 cartas de mayor
-- puntaje para envido (0 las "negras" y su número el resto)y les suma 20
sumaPuntosEnvido :: [Carta] -> Int
sumaPuntosEnvido [] = 0
sumaPuntosEnvido xs
    | length xs == 1 = (valorPEnvido . head) xs
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
    | c2 > c1 = c2
    | c1 > c2 = c1
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

-- Definimos que opciones imprimirle al usuario en base a los cantos de envido, los cantos de truco
-- y las cartas jugadas y disponibles
-- Recibe: CantosEnvido -> CantosTruco -> Jugadas -> CartasDisponibles
-- Devuelve: Lista de opciones
opciones :: [(Carta,Carta)] -> [Carta] -> [String] -> [String] -> [String]
opciones csJugadas csDisp csEnvido csTruco = do
        let opc = opcionesEnvido csEnvido csTruco csJugadas ++ opcionesTruco csEnvido csTruco ++ opcionesCartas csEnvido csTruco csDisp ++ ["Salir"]
        zipWith (++) [(++ " - ") (show x) | x <- iterate (+ 1) 1] opc

-- Generamos las opciones de canto de envido disponibles en base a los cantos previos de envido
-- verificando que no se haya jugado ya el envido, que no se esté cantando truco (salvo por el primer canto)
-- y verificando que no se haya jugado ya la primera mano de cartas
opcionesEnvido :: [String] -> [String] -> [(Carta,Carta)] -> [String]
opcionesEnvido csEnvido csTruco csJugadas
    | "JUGADO" `elem` csEnvido = []
    | null csEnvido && (null csTruco || length csTruco == 1) && (null csJugadas || (fst (head csJugadas) == nada)) = map show [ENVIDO, REALENVIDO, FALTAENVIDO]
    | not (null csEnvido) = tail $ dropWhile (/= last csEnvido) respuestasEnvido
    | otherwise = []

-- Generamos las opciones de canto de truco controlando que no se esté en medio de un canto de envido
opcionesTruco :: [String] -> [String] -> [String]
opcionesTruco csEnvido csTruco
    | (not (null csEnvido) && "JUGADO" `notElem` csEnvido)  || ("CANTADO" `elem` csTruco) = []
    | null csTruco = [show TRUCO]
    | otherwise =  take 1 (reverse(takeWhile (/= last csTruco) (reverse (map show [TRUCO, RETRUCO, VALECUATRO])))) ++ map show [QUIERO, NOQUIERO]

-- Generamos las ociones de cartas a jugar cuando es posible realizar una jugada
opcionesCartas :: [String] -> [String] -> [Carta] -> [String]
opcionesCartas csEnvido csTruco csDisp
    | not (null csEnvido) && "JUGADO" `notElem` csEnvido = []
    | not (null csTruco) && "CANTADO" `notElem` csTruco = []
    | otherwise = map (("Jugar "++).show) csDisp ++ ["Retirarse"]

