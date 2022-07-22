from itertools import dropwhile, takewhile, count
from operator import concat
from random import shuffle
from mazo import cartaMismoPalo, mismoPalo, mostrarCarta, valorPEnvido, Nada, cartaEsMayor, Mazo

def take(i,xs):
    if not xs : return []
    elif i<1 : return []
    else : return [xs[0]] + take(i-1,dropwhile(lambda x: x == xs[0],xs))

def swap(t):
    t1 = t[1]
    t2 = t[0]
    return (t1,t2)

TRUCO = "Truco"
RETRUCO = "Re Truco"
VALECUATRO = "Vale Cuatro"
ENVIDO = "Envido"
ENVIDOENVIDO = "Envido Envido"
REALENVIDO = "Real Envido"
FALTAENVIDO = "Falta Envido"
QUIERO = "Quiero"
NOQUIERO = "No quiero"

respuestasEnvido = [ENVIDO, ENVIDOENVIDO, REALENVIDO, FALTAENVIDO, QUIERO, NOQUIERO]

def agruparPorPalo(cs):
    if not cs : return []
    elif len(cs) == 1 : return [cs]
    elif len(cs) == 2 and not cartaMismoPalo(cs[0],cs[1]) : return [[cs[0]],[cs[1]]]
    else : 
        l1 = [c for c in cs if cartaMismoPalo(c,cs[0])]
        l2 = [c for c in cs if not cartaMismoPalo(c,cs[0])]
        return [l1]+agruparPorPalo(l2)

def sumaPuntosEnvido(cs):
    if not cs : return 0
    elif len(cs) == 1 : return valorPEnvido(cs[0])
    elif len(cs) >= 3 : return sumaPuntosEnvido(list(filter(lambda c: min(map(valorPEnvido,cs)) < valorPEnvido(c), cs)))
    else : return sum(map(valorPEnvido,cs)) + 20

def puntosEnvidoMano(cs):
    if not cs : return 0
    else : 
        return max(list(map(sumaPuntosEnvido,agruparPorPalo(cs))))

def ganaCarta(c1,c2):
    if cartaEsMayor(c1,c2) : return c1
    elif cartaEsMayor(c2,c1) : return c2
    else : return Nada

def repartir():
    m = Mazo[:]
    shuffle(m)
    return (m[0:3],m[3:6])

def opcionesCartas(csEnvido,csTruco,cartas):
    if csEnvido and not "JUGADO" in csEnvido : return []
    elif csTruco and not "CANTADO" in csTruco : return []
    else : return list(map(lambda c: "Jugar "+mostrarCarta(c),cartas)) + ["Retirarse"]

def opcionesTruco(csEnvido,csTruco):
    if (csEnvido and not "JUGADO" in csEnvido) or ("CANTADO" in csTruco) : return []
    elif not csTruco : return [TRUCO]
    else : 
        l = list(takewhile(lambda c: c != csTruco[len(csTruco)-1],[VALECUATRO,RETRUCO,TRUCO]))
        l.reverse()
        return take(1,l) + [QUIERO,NOQUIERO]

def opcionesEnvido(csEnvido,csTruco,csJugadas):
    respuestas = respuestasEnvido[:]
    if "JUGADO" in csEnvido : return []
    elif not csEnvido and (not csTruco or len(csTruco) == 1) and (not csJugadas or csJugadas[0][0] == Nada) : return [ENVIDO, REALENVIDO, FALTAENVIDO]
    elif csEnvido : 
        respuestas.reverse()
        l = list(takewhile(lambda c: c != csEnvido[len(csEnvido)-1], respuestas))
        l.reverse()
        return l
    else : return []

def opciones(csJugadas, csDisp, csEnvido, csTruco):
    opc = opcionesEnvido(csEnvido,csTruco,csJugadas) + opcionesTruco(csEnvido,csTruco) + opcionesCartas(csEnvido, csTruco,csDisp) + ['Salir']
    return list(map(lambda x,y: x+" - "+y,list(map(str,range(1,len(opc)+1))),opc))
