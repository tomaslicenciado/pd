{-# LANGUAGE MultiWayIf #-}
import Interacciones
    ( puntosEnvidoMano, repartir, Canto(..), cola, opciones, ganaCarta )
import Mazo ( carta, Carta, Palo(..), nada, mazo )
import Data.List ( sort, isPrefixOf, isSubsequenceOf, intercalate )
import Data.Bifunctor (first)
import Data.Char ( isDigit )
import Data.Tuple ( swap )
import Control.Monad ( when )
import System.IO ( hFlush, stdout, stdin )

data Jugador = J {nombre :: String, cartas :: [Carta], puntos :: Int} deriving (Eq)

-- Definición de empate y nadie como jugadores
empate :: Jugador
empate = J "Empate" [] 0

nadie :: Jugador
nadie = J "Nadie" [] 0

-- jugadas de j1 y j2 , jugador j1, jugador j2, cantos envido, cantos truco. TODO EN ESE ORDEN
-- Invertir j1 y j2 invierte las jugadas. Los cantos no se invierten
data Datos = D {jugadas :: [(Carta,Carta)], jugadores :: (Jugador, Jugador), cantos :: ([String], [String]), mensaje :: String}

-- Función inicial y comienzo de juego
main :: IO()
main = do
    clearScreen
    putStrLn "Hola, bienvenidos a esta versión básica de Truco para 2 en Haskell"
    putStrLn "Ingrese nombre jugador 1:"
    sJ1 <- getLine
    putStrLn "Ingrese nombre jugador 2:"
    sJ2 <- getLine
    let cartas = repartir
        j1 = J sJ1 (fst cartas) 0
        j2 = J sJ2 (snd cartas) 0
    putStrLn "Comenzando Partida."
    putStrLn "Presione una tecla para continuar..."
    getLine
    ciclo (D [] (j1,j2) ([],[]) "")

-- Ciclo de jugadas donde se muestra información y opciones a los jugadores por cada acción que se realice
ciclo ::  Datos -> IO()
ciclo d = do
    clearScreen
    if partidaTerminada d 
        then do
            let winner = uncurry jMayorPuntaje (jugadores d)
            putStrLn "PARTIDA FINALIZADA" 
            putStrLn $ "Ganador: " ++ nombre winner
            putStrLn $ "Ganó con " ++ (show . puntos) winner ++ " puntos"
            putStrLn "Inicie de nuevo el juego para otra partida"
            putStrLn "Gracias por elegirnos. Adiós."
        else if mensaje d /= "MANO TERMINADA"
            then do
                let ops = uncurry (opciones (jugadas d) ((cartas . fst . jugadores) d)) (cantos d)
                imprimirDatos d
                putStrLn $ "Opciones jugador "++(nombre.fst.jugadores)d++":\n"++unlines ops
                hFlush stdout
                opc <- getLine
                when (null opc || not (all isDigit opc)) $ ciclo d
                if
                    opc == show (length ops)
                then
                    putStrLn "Lamentamos su partida.\nHasta luego"
                else do
                    let choice = head [x | x <- ops , opc `isPrefixOf` x]
                        d1 = accionar (drop 4 choice) d
                    ciclo $ controlFinMano d1
            else do
                imprimirDatos d
                let ops = ["1 - Repartir", "2 - Salir"]
                putStrLn $ "Opciones jugador "++(nombre.fst.jugadores)d++":\n"++unlines ops
                opc <- getLine
                when (null opc || not (all isDigit opc)) $ ciclo d
                if
                    opc == "2"
                then
                    putStrLn "Lamentamos su partida.\nHasta luego"
                else do
                    let cartas = repartir
                        j1 = J (nombre ((fst . jugadores) d)) (fst cartas) (puntos ((fst . jugadores) d))
                        j2 = J (nombre ((snd . jugadores) d)) (snd cartas) (puntos ((snd . jugadores) d))
                    ciclo $ D [] (j1, j2) ([], []) ""


-- Aplica una acción dependiendo de la elección de opción por parte del jugador
accionar :: String -> Datos -> Datos
accionar s d
    | "Retirarse" == s = sumarTrucoQuerido $ siguienteJugador $ D (jugadas d) (jugadores d) (cantos d) "MANO TERMINADA"
    | "Jugar " `isPrefixOf` s = do
        let d1 = D (jugarCarta (drop 6 s) ((fst . jugadores) d) (jugadas d)) (first (removerCarta (drop 6 s)) (jugadores d)) (cantos d) ""
        if (fst . last . jugadas) d1 == uncurry ganaCarta ((last . jugadas) d1) && (snd . last . jugadas) d1 /= nada
            then d1
            else siguienteJugador d1
    | (s == show ENVIDO) && ((snd . cantos) d == [show TRUCO]) = siguienteJugador $ D (jugadas d) (jugadores d) ([show ENVIDO],[]) ""
    | show NOQUIERO `isSubsequenceOf` s && not (null ((snd . cantos) d)) = D [] (jugadores (sumarTrucoNoQuerido d)) (cantos d) "MANO TERMINADA"
    | "uiero" `isSubsequenceOf` s = do
        let envidos = (fst . cantos) d
            trucos = (snd . cantos) d
        if not (null envidos) && notElem "JUGADO" envidos
            then despuesDeCanto $ resultadoEnvido (D (jugadas d) (jugadores d) ((fst . cantos) d ++ [s,"JUGADO"], (snd . cantos) d) "")
            else despuesDeCanto $ D (jugadas d) (jugadores d) ((fst . cantos) d, (snd . cantos) d ++ [s,"CANTADO"]) ""
    | "Envido" `isSubsequenceOf` s = siguienteJugador $ D (jugadas d) (jugadores d) ((fst . cantos) d ++ [s], (snd . cantos) d) ""
    | otherwise = siguienteJugador $ D (jugadas d) (jugadores d) ((fst . cantos) d, (snd . cantos) d ++ [s]) ""

-- Ingresa la carta elegida al monto de cartas jugadas. 
jugarCarta :: String -> Jugador -> [(Carta,Carta)] -> [(Carta, Carta)]
jugarCarta carta j jugadas
    | null jugadas = [(head [x | x <- cartas j, show x == carta], nada)]
    | length jugadas < 3 = do
        let uj = (last . map fst) jugadas
        if uj == nada
            then init jugadas ++ [(head [x | x <- cartas j, show x == carta], (last . map snd) jugadas)]
            else jugadas ++ [(head [x | x <- cartas j, show x == carta], nada)]
    | length jugadas == 3 && (last . map fst) jugadas == nada = init jugadas ++ [(head [x | x <- cartas j, show x == carta], (last . map snd) jugadas)]
    | otherwise = jugadas

-- Quita la carta jugada del monto del jugador
removerCarta :: String -> Jugador -> Jugador
removerCarta c j = J (nombre j) ([x | x <- cartas j, show x /= c]) (puntos j)

-- Imprime el estado de la partida en pantalla
imprimirDatos :: Datos -> IO ()
imprimirDatos d = do
    putStrLn "                     PARTIDA EN CURSO"
    putStrLn $ "Jugador actual: " ++ (nombre . fst . jugadores) d
    putStrLn $ unlines $ map show ((cartas . fst . jugadores) d)
    putStrLn $ "Puntos para el envido: " ++ show (puntosEnvidoMano ((cartas . fst . jugadores) d ++ (map fst . jugadas) d))
    putStrLn "\nJugadas:"
    putStrLn $ intercalate "" ((nombre . fst . jugadores) d : replicate (30- length ((nombre . fst . jugadores) d)) " " ++ [(nombre . snd . jugadores) d])
    putStrLn $ uncurry mostrarCartas (unzip (jugadas d))
    putStrLn "\nCantos:"
    putStrLn $ intercalate " -> " ((fst . cantos) d)
    putStrLn $ intercalate " -> " ((snd . cantos) d)
    putStrLn ""
    putStrLn $ "Puntos " ++ (nombre . fst . jugadores) d ++ ": " ++ (show . puntos. fst. jugadores) d
    putStrLn $ "Puntos " ++ (nombre . snd . jugadores) d ++ ": " ++ (show . puntos. snd. jugadores) d
    putStrLn ""
    putStrLn $ mensaje d ++ "\n"

-- Limpia la pantalla
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

-- Función auxiliar para poder "extraer" un único elemento de una lista sin tener que preocuparse por listas vacías
-- Sólo para listas de Strings
unlistS :: [String] -> String
unlistS [] = ""
unlistS xs = head xs

mostrarCartas :: [Carta] -> [Carta] -> String
mostrarCartas [] [] = ""
mostrarCartas [] cs2 = intercalate "" (replicate 30 " " ++ [(show . head) cs2]) ++ "\n" ++ mostrarCartas [] (drop 1 cs2)
mostrarCartas cs1 [] = (show . head) cs1 ++ "\n" ++ mostrarCartas (tail cs1) []
mostrarCartas cs1 cs2 = intercalate "" ((show . head) cs1 : replicate (30-length ((show . head) cs1)) " " ++ [(show . head) cs2]) ++ "\n" ++ mostrarCartas (tail cs1) (tail cs2)


resultadoEnvido :: Datos -> Datos
resultadoEnvido d
    | show NOQUIERO `elem` (fst . cantos) d = D (jugadas d) ((fst . jugadores) d, J ((nombre . snd . jugadores) d) ((cartas . snd . jugadores )d) ((puntos . snd . jugadores) d + envidoNoQuerido ((fst . cantos) d ))) (cantos d) ""
    | otherwise = if puntosEnvidoMano ((cartas . fst . jugadores) d ++ map fst (jugadas d)) < puntosEnvidoMano ((cartas . snd . jugadores) d ++ map snd (jugadas d))
        then D (jugadas d) ((fst . jugadores) d, J ((nombre . snd . jugadores) d) ((cartas . snd . jugadores )d) ((puntos . snd . jugadores) d + envidoQuerido  d )) (cantos d) ((nombre.snd.jugadores)d++" GANA ENVIDO!")
        else D (jugadas d) (J ((nombre . fst . jugadores) d) ((cartas . fst . jugadores )d) ((puntos . fst . jugadores) d + envidoQuerido d ), (snd . jugadores) d) (cantos d) ((nombre.fst.jugadores)d++" GANA ENVIDO!")

envidoNoQuerido :: [String] -> Int
envidoNoQuerido cantos
    | length cantos == 3 = 1
    | length cantos == 4 && head cantos == show ENVIDO = 2
    | length cantos == 4 = 3
    | length cantos == 5 && notElem (show REALENVIDO) cantos = 4
    | length cantos == 5 = 5
    | length cantos == 6 = 7
    | otherwise = 0

envidoQuerido :: Datos -> Int
envidoQuerido d = do
    let envidos = (fst . cantos) d
        j1 = (fst . jugadores) d
        j2 = (snd . jugadores) d
    if show FALTAENVIDO `notElem` envidos
        then sum ([2 | x <- envidos, x == show ENVIDO]++[2 | x <- envidos, x == show ENVIDOENVIDO]++[3 | x <- envidos, x == show REALENVIDO])
        else if  puntos j1 < 15 && puntos j2 < 15
            then  30 - minimum [puntos j1, puntos j2]
            else 30 - maximum [puntos j1, puntos j2]

-- Paso turno al siguiente jugador
siguienteJugador :: Datos -> Datos
siguienteJugador d = D (map swap (jugadas d)) (swap (jugadores d)) (cantos d) (mensaje d)

-- Le devuelvo al jugador que corresponda el control de la partida
despuesDeCanto :: Datos -> Datos
despuesDeCanto d
    | null ((snd . cantos) d) && even (length ((fst . cantos) d) - 1) = siguienteJugador d
    | not (null ((snd . cantos) d)) && even (length ((snd . cantos) d) - 1) = siguienteJugador d
    | otherwise = d

-- El jugador 1 dijo no quiero y los puntos van al jugador 2
sumarTrucoNoQuerido :: Datos -> Datos
sumarTrucoNoQuerido d = D (jugadas d) ((fst . jugadores) d, J (nombre ((snd . jugadores) d)) (cartas ((snd . jugadores) d)) (puntos ((snd . jugadores) d) + length ((snd . cantos) d))) (cantos d) (mensaje d)

-- El jugador 1 es quien gana la mano y se lleva los puntos del truco
sumarTrucoQuerido :: Datos -> Datos
sumarTrucoQuerido d 
    | null ((snd . cantos) d) = D (jugadas d) (J (nombre ((fst . jugadores) d)) (cartas ((fst . jugadores) d)) (puntos ((fst . jugadores) d) + 1), (snd . jugadores) d) (cantos d) (mensaje d)
    | otherwise = D (jugadas d) (J (nombre ((fst . jugadores) d)) (cartas ((fst . jugadores) d)) (puntos ((fst . jugadores) d) + length ((snd . cantos) d) - 1), (snd . jugadores) d) (cantos d) (mensaje d)

controlFinMano :: Datos -> Datos
controlFinMano d = do
    let js = jugadas d
        gs = ganadores js (jugadores d)
    if ganador gs == nadie
        then d
        else if ganador gs == empate
            then D (jugadas d) (jugadores d) (cantos d) "MANO TERMINADA"
            else if ganador gs == (fst . jugadores) d
                    then sumarTrucoQuerido $ D (jugadas d) (jugadores d) (cantos d) "MANO TERMINADA"
                    else sumarTrucoQuerido $ siguienteJugador $ D (jugadas d) (jugadores d) (cantos d) "MANO TERMINADA"

ganadores :: [(Carta,Carta)] -> (Jugador, Jugador) -> [Jugador]
ganadores [] _ = []
ganadores ((c1,c2):cs) (j1,j2)
    | c1 == nada || c2 == nada = []
    | ganaCarta c1 c2 == nada = empate : ganadores cs (j1,j2)
    | ganaCarta c1 c2 == c1 = j1 : ganadores cs (j1,j2)
    | otherwise = j2 : ganadores cs (j1,j2)

ganador :: [Jugador] -> Jugador
ganador js
    | length js <= 1 = nadie
    | length js == 2 && length [x | x <- js, x == empate] == 1 = head [x | x <- js, x /= empate]
    | length js == 3 && length [x | x <- js, x == empate] == 1 = head [x | x <- js, x /= empate]
    | length js == 3 && length [x | x <- js, x == empate] == 2 = head [x | x <- js, x /= empate]
    | length js >=2 && empate `notElem` js = if length (filter (== head js) js) == length (filter (/= head js) js)
        then nadie
        else head (head [xs | xs <- [filter (== head js) js, filter (/= head js) js], length xs >= length (filter (== head js) js)])
    | otherwise = empate

partidaTerminada :: Datos -> Bool
partidaTerminada d = (puntos . fst . jugadores) d >= 30 || (puntos . snd . jugadores) d >= 30

jMayorPuntaje :: Jugador -> Jugador -> Jugador
jMayorPuntaje j1 j2 = if puntos j1 >= puntos j2 then j1 else j2