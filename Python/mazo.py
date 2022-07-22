Oros = "Oros"
Espadas = "Espadas"
Basto = "Basto"
Copas = "Copas"
Cualquier = "Cualquier"

def mismoPalo(a,b):
    if a == Cualquier or b == Cualquier : return True
    else : return a == b

As = 1
Sota = 10
Caballo = 11
Rey = 12

def Carta(n, p):
    if n in [*(range(As,8)),*(range(Sota,Rey+1))] and p in [Oros,Espadas,Basto,Copas, Cualquier] : return {'numero' : n, 'palo' : p}
    else : return {'numero' : 0, 'palo' : Cualquier}

Macho = Carta(As,Espadas)
Hembra = Carta(As, Basto)
AnchoOro = Carta(As, Oros)
AnchoCopa = Carta(As, Copas)
Nada = Carta(0,Cualquier)

def mostrarCarta(c):
    if c == Macho : return "Macho"
    elif c == Hembra : return "Hembra"
    elif c == AnchoOro : return "Ancho de Oros"
    elif c == AnchoCopa : return "Ancho de Copas"
    elif c == Nada : return ""
    elif c['numero'] == As : return "As de " + c['palo']
    elif c['numero'] == Sota : return "Sota de " + c['palo']
    elif c['numero'] == Caballo : return "Caballo de " + c['palo']
    elif c['numero'] == Rey : return "Rey de " + c['palo']
    else : return str(c['numero']) + " de " + c['palo']

Orden = [Macho, Hembra, Carta(7,Espadas), Carta(7,Oros), Carta(3,Cualquier), Carta(2,Cualquier),
        Carta(Rey,Cualquier), Carta(Caballo,Cualquier), Carta(7,Cualquier), Carta(6,Cualquier),
        Carta(5,Cualquier), Carta(4,Cualquier), Nada]

Mazo = [Carta(x,y) for x in [*(range(As,8)),*(range(Sota,Rey+1))] for y in [Oros,Espadas,Basto,Copas]]

def cartasIguales(a,b):
    return a['numero'] == b['numero'] and cartaMismoPalo(a,b)

def cartaEsMayor(a,b):
    i1 = 0
    i2 = 0
    for carta in Orden :
        if cartasIguales(a,carta) : i1 = Orden.index(carta)
        if cartasIguales(b,carta) : i2 = Orden.index(carta)
    return i1 < i2

def valorPEnvido(c):
    if c['numero'] < Sota : return c['numero']
    else : return 0

def cartaMismoPalo(c1,c2):
    return mismoPalo(c1['palo'],c2['palo'])