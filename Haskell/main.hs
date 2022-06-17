{-# LANGUAGE MultiWayIf #-}
import System.IO (openFile, hGetContents, IOMode (ReadMode), hClose)
import Control.Monad
import Cartas
import CartaVSCarta
import Manos
import Data.List (intercalate, isInfixOf)

main :: IO ()
main = do
    putStrLn "Hola, bienvenidos a esta versión básica de Truco para 2 en Haskell"
    putStrLn "Indique el nombre del jugador 1"
    j1 <- getLine
    putStrLn "Indique el nombre del jugador 2"
    j2 <- getLine

-- main = do
--     putStrLn "Hola, bienvenidos a esta versión básica de Truco en Haskell"
--     putStrLn "Indíquenos su nombre:"
--     name <- getLine
--     let partida = iniciarPartida name
--     jugada "Primera mano" partida
--     putStrLn "Adiós"




-- clearScreen :: IO ()
-- clearScreen = putStr "\ESC[2J\ESC[H"


-- data Partida = P {cantos :: ([String],[String]), cartas :: [Carta], repartidas :: ([Carta], [Carta]),
--                     jugadas :: [(Carta, Carta)], puntaje :: ([Int], [Int]), jugador :: String,
--                     cantosMano :: [String]}

-- instance Show Partida where
--     show p = do
--         let s1 = "Jugando al Truco con " ++ jugador p ++ "\n\n"
--             s2 = s1 ++ "Puntos PC: " ++ show (sum (fst (puntaje p))) ++ "\n"
--             s3 = s2 ++ "Puntos jugador: " ++ show (sum (snd (puntaje p))) ++ "\n"
--             s4 = s3 ++ "Cartas jugadas:\n" ++ "PC      | " ++ (concatMap (\n -> show n ++ " | ") [x | (x,_) <- jugadas p]) ++ "\n"
--             s5 = s4 ++ "Jugador | " ++ (concatMap (\n -> show n ++ " | ") [x | (_,x) <- jugadas p]) ++ "\n"
--             s6 = s5 ++ "Cantos PC: " ++ intercalate ", "  (fst $ cantos p) ++ "\n"
--             s7 = s6 ++ "Cantos jugador: " ++ intercalate ", " (snd $ cantos p) ++ "\n\n"
--             s8 = s7 ++ "Cartas jugador: " ++ intercalate ", " (map show (snd $ repartidas p)) ++ "\n\n"
--             salida = s7 ++ "Opciones: "
--         salida

-- iniciarPartida :: String -> Partida
-- iniciarPartida name = P ([],[]) mazo (splitAt 3 (repartir 6 (name++"Truc0$987123") mazo)) [] ([],[]) name []

-- jugada :: String -> Partida -> IO ()
-- jugada estado partida = do
--     print partida
--     if 
--         | estado == "Primera mano" -> putStrLn "1 - Jugar carta | 2 - Cantar envido | 3 - Cantar truco | 9 - Salir"
--         | estado == "PC canta envido" -> putStrLn "1 - Quiero | 2 - Envido | 3 - Real Envido | 4 - Falta Envido | 5 - No se quiere | 9 - Salir"
--         | estado == "PC canta envido envido" -> putStrLn "1 - Quiero | 2 - Falta Envido | 3 - No se quiere | 9 - Salir"
--         | estado == "PC canta real envido" -> putStrLn "1 - Quiero | 2 - Falta Envido | 3 - No se quiere | 9 - Salir"
--         | estado == "PC canta falta envido" -> putStrLn "1 - Quiero | 2 - No se quiere | 9 - Salir"
--         | estado == "PC canta truco" -> if
--                                         any (isInfixOf "ENVIDO") (uncurry (++) (cantos partida))
--                                         then
--                                             putStrLn "1 - Quiero | 2 - Quiero Retruco | 3 - No se quiere | 9 - Salir"
--                                         else
--                                             putStrLn "1 - Quiero | 2 - Quiero Retruco | 3 - No se quiere | 4 - Envido va primero | 9 - Salir"
--         | estado == "PC canta retruco" -> putStrLn ""
--         | otherwise -> putStrLn "9 - Salir"



-- puntos en 0
-- repartir cartas
-- empieza jugador
--      tira carta
--          cantar envido
--              responde
--          responder o jugar envido
--      tirar carta
--          tira carta o canta truco
--      
--
--  acción
--  mensaje