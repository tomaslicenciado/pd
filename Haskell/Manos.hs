{-# LANGUAGE MultiWayIf #-}
module Manos(

)where

import Cartas 
import CartaVSCarta 


cantaTruco :: String
cantaTruco = "TRUCO!"

cantaEnvido :: String
cantaEnvido = "ENVIDO"

cantaEnvidoEnvido :: String
cantaEnvidoEnvido = "ENVIDO ENVIDO"

envidoVaPrimero :: String
envidoVaPrimero = "ENVIDO VA PRIMERO"

cantaRealEnvido :: String
cantaRealEnvido = "REAL ENVIDO"

cantaLaFalta :: String
cantaLaFalta = "FALTA ENVIDO"

cantaReTruco :: String
cantaReTruco = "QUIERO RETRUCO"

cantaVale4 :: String
cantaVale4 = "QUIERO VALE 4"

respuestasEnvido :: [String]
respuestasEnvido = ["QUIERO", cantaEnvidoEnvido, cantaRealEnvido, cantaLaFalta, "NO SE QUIERE"]
--respuestasEnvido = ["QUIERO", "ENVIDO ENVIDO", "REAL ENVIDO", "FALTA ENVIDO", "NO SE QUIERE"]

envido :: String -> [Carta] -> String
envido msj cartas
    | msj == cantaEnvido = if
        | puntosEnvido cartas < 24 -> "NO SE QUIERE"
        | puntosEnvido cartas < 27 -> "QUIERO"
        | puntosEnvido cartas < 29 -> cantaRealEnvido
        | puntosEnvido cartas < 32 -> cantaEnvidoEnvido
        | otherwise -> cantaLaFalta
    | msj == cantaRealEnvido = if
        | puntosEnvido cartas < 27 -> "NO SE QUIERE"
        | puntosEnvido cartas < 32 -> "QUIERO"
        | otherwise -> cantaLaFalta
    | msj == cantaEnvidoEnvido = if
        | puntosEnvido cartas < 29 -> "NO SE QUIERE"
        | puntosEnvido cartas < 32 -> "QUIERO"
        | otherwise -> cantaLaFalta
    | msj == cantaLaFalta = if puntosEnvido cartas < 32 then "NO SE QUIERE" else "QUIERO"
    | otherwise = "NO SE QUIERE"

truco :: String -> [Carta] -> [(Carta, Carta)] -> String
truco msj cartas jugadas
    | msj == cantaTruco = if
        | length cartas == 3 -> if 
                | maximum cartas >= carta 7 Oros -> cantaReTruco
                | maximum cartas >= carta 2 Cualquier -> "QUIERO"
                | otherwise -> "NO SE QUIERE"
        | length cartas == 2 -> (if ganaJugador (head jugadas) then 
                                    (if
                                    | maximum cartas >= hembra -> cantaReTruco
                                    | maximum cartas >= carta 7 Oros -> "QUIERO"
                                    | otherwise -> "NO SE QUIERE")
                                else truco msj (cartas++[fst(head jugadas)]) jugadas)
        | length cartas == 1 -> if maximum cartas >= carta 7 Espadas then "QUIERO" else "NO SE QUIERE"
        | otherwise -> "NO SE QUIERE"
    | msj == cantaReTruco = if
        | ganaJugador (head jugadas) -> if maximum cartas >= hembra then "QUIERO" else "NO SE QUIERE"
        | maximum cartas >= hembra -> cantaVale4
        | maximum cartas >= carta 7 Oros -> "QUIERO"
        | otherwise -> "NO SE QUIERE"
    | msj == cantaVale4 = if maximum cartas >= hembra then "QUIERO" else "NO SE QUIERE"
    | otherwise = "NO SE QUIERE"

ganaJugador :: (Carta, Carta) -> Bool
ganaJugador (x,nada) = False
ganaJugador (nada, y) = y == macho
ganaJugador (x,y) = y == ganaCarta x y

posibleRespuestaEnvido :: String -> [String]
posibleRespuestaEnvido "" = []
posibleRespuestaEnvido s = (head respuestasEnvido) : drop (head $ elemIndices s respuestasEnvido) (tail respuestasEnvido)
