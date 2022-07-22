from functools import reduce
from itertools import repeat
from operator import concat
from mazo import Nada, mostrarCarta
from interacciones import ENVIDO, ENVIDOENVIDO, FALTAENVIDO, NOQUIERO, REALENVIDO, TRUCO, ganaCarta, opciones, puntosEnvidoMano, repartir, swap, take
from os import name as sysname, system

def Jugador(n,cs,p):
    return {'nombre':n, 'cartas':cs, 'puntos':p}

empate = Jugador("Empate",[],0)
nadie = Jugador("Nadie",[],0)

def Datos(cjs,js,cs,msj):
    return {'jugadas': cjs, 'jugadores': js, 'cantos': cs, 'mensaje': msj}

def jMayorPuntaje(j1,j2):
    if j1['puntos'] >= j2['puntos'] : return j1
    else : return j2

def partidaTerminada(d):
    return d['jugadores'][0]['puntos'] >= 30 or d['jugadores'][1]['puntos'] >= 30

def ganador(js):
    if len(js) <= 1 : return nadie
    elif len(js) == 2 and js.count(empate) == 1 : return [j for j in js if j != empate][0]
    elif len(js) == 3 and js.count(empate) == 1 : return [j for j in js if j != empate][0]
    elif len(js) == 3 and js.count(empate) == 2 : return [j for j in js if j != empate][0]
    elif len(js) >= 2 and js.count(empate) == 0 :
        j1 = js[0]
        j2 = nadie
        js2 = [j for j in js if j != j1]
        if not js2 : return j1
        else : j2 = js2[0]
        if js.count(j1) == js.count(j2) : return nadie
        else : return [j for j in js if js.count(j) == max([js.count(j1), js.count(j2)])][0]
    elif len(js) == 3 and js.count(empate) == 3 : return empate
    else : return nadie

def ganadores(cs,js):
    cs1 = cs[:]
    if not cs : return []
    elif cs[0][0] == Nada or cs[0][1] == Nada : return []
    elif ganaCarta(cs[0][0],cs[0][1]) == Nada : 
        cs1.pop(0)
        return [empate] + ganadores(cs1,js)
    elif ganaCarta(cs[0][0],cs[0][1]) == cs[0][0] : 
        cs1.pop(0)
        return [js[0]] + ganadores(cs1,js)
    else : 
        cs1.pop(0)
        return [js[1]] + ganadores(cs1,js)

def sumarTrucoQuerido(d):
    if not d['cantos'][1] : d['jugadores'][0]['puntos'] += 1
    else : d['jugadores'][0]['puntos'] += len(d['cantos'][1]) - 1

def sumarTrucoNoQuerido(d):
    d['jugadores'][1]['puntos'] += len(d['cantos'][1])

def siguienteJugador(d):
    d['jugadores'] = swap(d['jugadores'])
    d['jugadas'] = list(map(swap,d['jugadas']))


def controlFinMano(d):
    gs = ganadores(d['jugadas'],d['jugadores'])
    if ganador(gs) == nadie : return d
    elif ganador(gs) == empate : d['mensaje'] = "MANO TERMINADA"
    elif ganador(gs) == d['jugadores'][0] : 
        d['mensaje'] = "MANO TERMINADA"
        sumarTrucoQuerido(d)
    elif ganador(gs) == d['jugadores'][1] : 
        d['mensaje'] = "MANO TERMINADA"
        sumarTrucoQuerido(siguienteJugador(d))

def despuesDeCanto(d):
    if not d['cantos'][1] and (len(d['cantos'][0])-1 )% 2 == 0 : siguienteJugador(d)
    elif d['cantos'][1] and (len(d['cantos'][1])-1) % 2 == 0 : siguienteJugador(d)

def envidoQuerido(d):
    envidos = d['cantos'][0]
    j1 = d['jugadores'][0]
    j2 = d['jugadores'][1]
    if not FALTAENVIDO in envidos : return sum([2 for x in envidos if x == ENVIDO]+
                                                [2 for x in envidos if x == ENVIDOENVIDO]+
                                                [3 for x in envidos if x == REALENVIDO])
    elif j1['puntos'] < 15 and j2['puntos'] < 15 : return 30 - min(j1['puntos'],j2['puntos'])
    else : return 30 - max(j1['puntos'],j2['puntos'])

def envidoNoQuerido(cs):
    if len(cs) == 3 : return 1
    elif len(cs) == 4 and cs[0] == ENVIDO : return 2
    elif len(cs) == 4 : return 3
    elif len(cs) == 5 and not REALENVIDO in cs : return 4
    elif len(cs) == 5 : return 5
    elif len(cs) == 6 : return 7
    else : return 0

def resultadoEnvido(d):
    if NOQUIERO in d['cantos'][0] : d['jugadores'][1]['puntos'] += envidoNoQuerido(d['cantos'][0])
    elif puntosEnvidoMano(d['jugadores'][0]['cartas']+list(map(lambda x:take(1,list(x)),d['jugadas']))) < puntosEnvidoMano(d['jugadores'][1]['cartas']+list(map(lambda x:take(1,list(x)),d['jugadas']))) :
        d['jugadores'][1]['puntos'] += envidoQuerido(d)
        d['mensaje'] = d['jugadores'][1]+" GANA ENVIDO"
    else : 
        d['jugadores'][0]['puntos'] += envidoQuerido(d)
        d['mensaje'] = d['jugadores'][0]['nombre']+" GANA ENVIDO"

def mostrarCartas(dcs1,dcs2):
    cs1 = dcs1
    cs2 = dcs2
    if not cs1 and not cs2 : return ""
    elif not cs1 and cs2 : 
        c2 = cs2.pop(0)
        return reduce(concat, list(repeat(" ",30)))+mostrarCarta(c2)+"\n"+mostrarCartas(cs1,cs2)
    elif cs1 and not cs2 : 
        c1 = cs1.pop(0)
        return mostrarCarta(c1)+"\n"+mostrarCartas(cs1,cs2)
    else : 
        c1 = cs1.pop(0)
        c2 = cs2.pop(0)
        return mostrarCarta(c1)+reduce(concat,list(repeat(" ",30 - len(mostrarCarta(c1)))))+mostrarCarta(c2)+"\n"+mostrarCartas(cs1,cs2)

def clearScreen(): #Definimos la función estableciendo el nombre que queramos
    if sysname == "posix":
        system ("clear")
    elif sysname == "ce" or sysname == "nt" or sysname == "dos":
        system ("cls")

def imprimirDatos(d):
    print("                            PARTIDA EN CURSO")
    print("Jugador actual: "+d['jugadores'][0]['nombre'])
    for carta in d['jugadores'][0]['cartas'] : print(mostrarCarta(carta))
    print("Puntos para el envido: "+str(puntosEnvidoMano(d['jugadores'][0]['cartas']+list(map(lambda x:x[0],d['jugadas'])))))
    print("\nJugadas:")
    print(d['jugadores'][0]['nombre']+reduce(concat,list(repeat(" ",30-len(d['jugadores'][0]['nombre']))))+d['jugadores'][1]['nombre'])
    print(mostrarCartas(list(map(lambda t: t[0],d['jugadas'])),list(map(lambda t: t[1],d['jugadas']))))
    print("\nCantos:")
    if d['cantos'][0] : print(reduce(lambda x,y:x+" -> "+y,d['cantos'][0]))
    else : print("")
    if d['cantos'][1] : print(reduce(lambda x,y:x+" -> "+y,d['cantos'][1]))
    else : print("")
    print(" ")
    print("Puntos "+d['jugadores'][0]['nombre']+": "+str(d['jugadores'][0]['puntos']))
    print("Puntos "+d['jugadores'][1]['nombre']+": "+str(d['jugadores'][1]['puntos']))
    print(" ")
    print(d['mensaje']+"\n")

def jugarCarta(sc, d):
    if not d['jugadores'][0]['cartas'] : return
    ls = list(map(mostrarCarta,d['jugadores'][0]['cartas']))
    if not sc in ls : return
    i = ls.index(sc)
    c = d['jugadores'][0]['cartas'].pop(i)
    if not d['jugadas'] : 
        d['jugadas'] = [(c,Nada)]
    elif len(d['jugadas']) < 3 :
        ultima_jugada = d['jugadas'][len(d['jugadas'])-1][0]
        if ultima_jugada == Nada : d['jugadas'][len(d['jugadas'])-1] = (c,d['jugadas'][len(d['jugadas'])-1][1])
        else : d['jugadas'] = d['jugadas'] + [(c,Nada)]
    elif len(d['jugadas']) == 3 and d['jugadas'][len(d['jugadas'])-1][0] == Nada : d['jugadas'][len(d['jugadas'])-1] = (c,d['jugadas'][len(d['jugadas'])-1][1])

def accionar(s,d):
    envidos = d['cantos'][0]
    trucos = d['cantos'][1]
    if s == "Retirarse" : 
        d['mensaje'] = "MANO TERMINADA"
        siguienteJugador(d)
        sumarTrucoQuerido(d)
    elif s.startswith("Jugar") :
        jugarCarta(s[6:],d)
        ultJug = d['jugadas'][len(d['jugadas'])-1]
        if ultJug[1] == Nada or ultJug[0] != ganaCarta(ultJug[0],ultJug[1]) :
            siguienteJugador(d)
        elif ultJug[1] == Nada : 
            siguienteJugador(d)
    elif s == ENVIDO and trucos==[TRUCO] : 
        d['cantos'] = ([ENVIDO],[])
        siguienteJugador(d)
    elif NOQUIERO in s and trucos : 
        d['mensaje'] = "MANO TERMINADA"
        sumarTrucoNoQuerido(d)
    elif "uiero" in s:
        if envidos and not "JUGADO" in envidos : 
            envidos = envidos + [s,"JUGADO"]
            d['cantos'] = (envidos,trucos)
            resultadoEnvido(d)
            despuesDeCanto(d)
        else :
            trucos = trucos + [s,"CANTADO"]
            d['cantos'] = (envidos,trucos)
            despuesDeCanto(d)
    elif "Envido" in s :
        envidos = envidos + [s]
        d['cantos'] = (envidos,trucos)
        siguienteJugador(d)
    else : 
        trucos = trucos + [s]
        d['cantos'] = (envidos,trucos)
        siguienteJugador(d)

def ciclo(d):
    clearScreen()
    jugador = d['jugadores'][0]
    if partidaTerminada(d) :
        winner = jMayorPuntaje(d['jugadores'][0],d['jugadores'][1])
        print("PARTIDA FINALIZADA")
        print("Ganador: "+winner['nombre'])
        print("Ganó con "+str(winner['puntos'])+" puntos")
        print("Inicie de nuevo el juego para otra partida")
        print("Gracias por elegirnos. Adiós")
    elif d['mensaje'] != "MANO TERMINADA" :
        ops = opciones(d['jugadas'],jugador['cartas'],d['cantos'][0],d['cantos'][1])
        imprimirDatos(d)
        print("Opciones jugador "+jugador['nombre'])
        print(reduce(lambda x,y:x+"\n"+y,ops))
        opc = input("Seleccione una ocpión: ")
        if not opc or not opc.isdigit() : ciclo(d)
        if opc == str(len(ops)) :
            print("Lamentamos su partida. Hasta luego.")
        elif int(opc)<len(ops) :
            choice = ops[int(opc)-1]
            accionar(choice[4:],d)
            controlFinMano(d)
            ciclo(d)
        else : 
            input("Opcion incorrecta. Presione cualquier tecla y elija una opción correcta")
            ciclo(d)
    else :
        imprimirDatos(d)
        ops = ["1 - Repartir", "2 - Salir"]
        print("Opciones jugador "+jugador['nombre'])
        print(reduce(lambda x,y:x+"\n"+y,ops))
        opc = input("Seleccione una ocpión: ")
        if not opc or not opc.isdigit() : ciclo(d)
        if opc == "2" :
            print("Lamentamos su partida. Hasta luego.")
        elif opc == "1" :
            cartas = repartir()
            jugador['cartas'] = cartas[0]
            jugador2 = d['jugadores'][1]
            jugador2['cartas'] = cartas[1]
            ciclo(Datos([],(jugador2,jugador),([],[]),""))



if __name__ == '__main__' :
    clearScreen()
    print("Hola, bienvenidos a esta versión básica de Truco para 2 en Python")
    nj1 = input("Ingrese el nombre para el jugador 1: ")
    nj2 = input("Ingrese el nombre para el jugador 2: ")
    cartas = repartir()
    j1 = Jugador(nj1,cartas[0],0)
    j2 = Jugador(nj2,cartas[1],0)
    print("Comenzando la partida")
    input("Presione una tecla para continuar...")
    ciclo(Datos([],(j1,j2),([],[]),""))
