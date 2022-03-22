{-# LANGUAGE MultiWayIf #-}
import Interacciones
    ( puntosEnvidoMano, repartir, Canto(..), mejorCarta, cola )
import Mazo ( carta, Carta, Palo(..), nada )
import Data.List ( sort )
import Data.Bifunctor (first)

data Jugador = J {nombre :: String, cartas :: [Carta], puntos :: Int}

jugada :: Int -> Jugador -> Jugador -> [Canto] -> ([Carta],[Carta]) -> (Int, Int) -> Jugador
jugada opcion j1 j2 cantos jugadas puntos
    | opcion == 0 = j2
    | otherwise = j1

opciones :: [Carta] -> [Canto] -> String
opciones cartas cantos
    | length cartas + length cantos == 0 = ""
    | not (null cartas) = ""
    | otherwise = ""



-- juegaPC :: Int -> Jugador -> Jugador -> [Canto] -> ([Carta],[Carta]) -> (Int,Int) -> Jugador
-- juegaPC vuelta jpc jugador cantos jugadas pts
--     | vuelta == 0 = if
--         | null (cartas jpc) -> do
--                         let c1 = repartir
--                             j1 = J (nombre jpc) (fst c1) (puntos jpc)
--                             j2 = J (nombre jugador) (snd c1) (puntos jugador)
--                         juegaPC 0 j1 j2 cantos jugadas pts
--         | puntosEnvidoMano (cartas jpc) >= 27 -> juegaJugador 1 jpc jugador (ENVIDO:cantos) jugadas pts
--         | maximum (cartas jpc) >= carta 7 Oros -> juegaJugador 1 jpc jugador (TRUCO:cantos) jugadas pts
--         | otherwise -> do
--             let c1 = (tail . sort . cartas) jpc
--                 j1 = J (nombre jpc) c1 (puntos jpc)
--             juegaJugador 1 j1 jugador (SILENCIO:cantos) (first ((minimum . cartas) jpc :) jugadas) pts
--     | vuelta == 1 = if head cantos == SILENCIO then if
--         | puntosEnvidoMano (cartas jpc) >= 27 -> juegaJugador 1 jpc jugador (ENVIDO:cantos) jugadas pts
--         | maximum (cartas jpc) >= carta 7 Espadas -> juegaJugador 1 jpc jugador (TRUCO:cantos) jugadas pts
--         | mejorCarta (cartas jpc) jugadas /= nada -> do
--             let c1 = mejorCarta (cartas jpc) jugadas
--                 cs = takeWhile (/=c1) (cartas jpc) ++ cola (dropWhile (/= c1) (cartas jpc))
--                 j1 = J (nombre jpc) cs (puntos jpc)
--             juegaJugador 2 j1 jugador (SILENCIO:cantos) (first (c1 :) jugadas) pts
--         | otherwise -> jpc
--         else jpc
--     |otherwise = jpc

-- juegaJugador :: Int -> Jugador -> Jugador -> [Canto] -> ([Carta],[Carta]) -> (Int,Int) -> Jugador
-- juegaJugador vuelta jpc jugador cantos jugadas pts = jugador