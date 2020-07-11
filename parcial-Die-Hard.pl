simonDice(irA(calle138, amsterdam, harlem), [mcClane]). 
simonDice(hacer(vestirCartel, harlem), [mcClane]).
simonDice(irA(west72nd, broadway, upperWestSide), [mcClane, zeus]).
simonDice(hacer(resolverAcertijo, upperWestSide), [mcClane, zeus]).
simonDice(irA(wallStreetStation, worldTradeCenter), [mcClane, zeus]).
simonDice(hacer(atenderTelefono, worldTradeCenter), [zeus]).
simonDice(irA(tompkinsSquarePark, eastVillage), [mcClane, zeus]).
simonDice(hacer(desarmarBomba, eastVillage), [mcClane, zeus]).
simonDice(irA(yankeeStadium, bronx), [zeus]).


% Punto 1.a
% pasoPor/2

% Relacionar a la persona y al lugar si esa persona estuvo en ese lugar.
% pasoPor(Persona, Lugar).
% Persona = mcClane
% Lugar = harlem


pasoPor(Persona, Lugar):-
    simonDice(irA(_, _, Lugar), Personas),
    member(Persona, Personas).
pasoPor(Persona, Lugar):-
    simonDice(irA(_, Lugar), Personas),
    member(Persona, Personas).
pasoPor(Persona, Lugar):-
    simonDice(hacer(_, Lugar), Personas),
    member(Persona, Personas).

% version 2
pasoPor2(Persona, Lugar):-
    simonDice(Accion, Personas),
    lugar(Accion, Lugar),
    member(Persona, Personas).

lugar(irA(_,_, Lugar), Lugar).
lugar(irA(_, Lugar), Lugar).
lugar(hacer(_, Lugar), Lugar).
    

% Punto 1.b
% lugaresPorLosQuePaso/2

% Relacionar a una persona con todos los lugares por los que pasó, sin repetir.

lugaresPorLosQuePaso(Persona, Lugares):-
    pasoPor(Persona, _),    
    findall(Lugar, pasoPor(Persona, Lugar), ListaLugares),
    list_to_set(ListaLugares, Lugares).

lugaresPorLosQuePaso2(Persona, Lugares):-
    pasoPor(Persona, _),    
    findall(Lugar, distinct(Lugar, pasoPor(Persona, Lugar)), Lugares).

%punto 2

explosion(sextaAvenida).
explosion(worldTradeCenter).
explosion(bote).
amenaza(chinatown, 5).
amenaza(upperWestSide, 2).
amenaza(escuelaDeArthur, 3).


% Punto 2.a

% Conocer a un posible culpable de atentado en un lugar, que es una persona que estuvo en un lugar 
% en el cual hubo una explosión.

posibleCulpable(Persona, Lugar):-
    pasoPor(Persona, Lugar),
    explosion(Lugar).

% Punto 2.b

% Conocer si un lugar dado explotó por culpa de McClane, 
% que se cumple cuando ese lugar explotó y McClane fue el único que estuvo ahí.

persona(Persona):-
    distinct(Persona, pasoPor(Persona, _)).
persona2(Persona):-
    pasoPor(Persona, _).

explotoPorCulpaDeMcClane(Lugar):-
    explosion(Lugar),
    findall(Persona, posibleCulpable(Persona, Lugar), Personas),
    list_to_set(Personas, [mcClane]).

explotoPorCulpaDeMcClane2(Lugar):-
    posibleCulpable(mcClane, Lugar),
    not((posibleCulpable(Persona, Lugar), Persona \= mcClane)).


% Punto 3

% Quién es afortunado, que es aquel que estuvo en lugares amenazados por un total 
% de al menos 8 horas (entre todos los lugares), pero en ninguno que haya explotado.

afortunado(Persona):- 
    pasoPor(Persona,_),
    not(posibleCulpable(Persona,_)),
    tiempoTotalAmenaza(Persona, TiempoTotal),
    TiempoTotal >= 8.

tiempoTotalAmenaza(Persona, TiempoTotal):-
    findall(Duracion,
        (pasoPor(Persona,Lugar),amenaza(Lugar,Duracion)), 
        Duraciones),
    sumlist(Duraciones,TiempoTotal).


% Punto 4

% Se los dejamos a ustedes :]

% Punto 5.a

% Elegimos hacer un functor: estado(LitrosDelChico, LitrosDelGrande)

% Punto 5.b

% Realizar un predicado que relacione una acción (o paso) con un estado de bidones anterior y un 
% estado de bidones posterior a realizar dicha acción: 

% - Ejemplo 1: si tengo los bidones vacíos y la acción que debo realizar es llenar el bidón chico, 
% lo que debe pasar es que el estado del bidón chico debe “cambiar”, mientras que el grande queda igual. 
% Por otro lado, teniendo en cuenta el enunciado, no sería posible realizar esa misma acción si el bidón 
% chico ya esta lleno.
% - Si tengo el grande lleno y el chico vacío y la acción es de pasar del grande al chico, entonces el chico 
% queda lleno, y el grande queda con el resto del agua que no cupo en el chico.

% PRO-TIP: Existen operadores min y max. Por ejemplo: N is min(4, 5)
% Este predicado solo requiere ser inversible para el estado posterior.

accion(llenarChico).
accion(llenarGrande).
accion(vaciarChico).
accion(vaciarGrande).
accion(ponerChicoEnGrande).
accion(ponerGrandeEnChico).	

hacer(llenarChico, estado(LitrosDelChico, LitrosDelGrande), estado(3, LitrosDelGrande)):-
    LitrosDelChico < 3.
hacer(llenarGrande, estado(LitrosDelChico, LitrosDelGrande), estado(LitrosDelChico, 5)):-
    LitrosDelGrande < 5.
hacer(vaciarChico, estado(LitrosDelChico, LitrosDelGrande), estado(0, LitrosDelGrande)):-
    LitrosDelChico > 0.
hacer(vaciarGrande, estado(LitrosDelChico, LitrosDelGrande), estado(LitrosDelChico, 0)):-
    LitrosDelGrande > 0.
hacer(ponerChicoEnGrande, estado(InicialDelChico, InicialDelGrande), estado(FinalDelChico, FinalDelGrande)):-
    InicialDelChico > 0,
    InicialDelGrande < 5,
    FinalDelGrande is min(5, InicialDelGrande + InicialDelChico),
    FinalDelChico is InicialDelChico - (FinalDelGrande - InicialDelGrande).

hacer(ponerGrandeEnChico, estado(InicialDelChico, InicialDelGrande), estado(FinalDelChico, FinalDelGrande)):-
    InicialDelGrande > 0,
    InicialDelChico < 3,
    FinalDelChico is min(3, InicialDelGrande + InicialDelChico),
    FinalDelGrande is InicialDelGrande - (FinalDelChico - InicialDelChico).


% Punto 5.c

% Pensar un predicado que relacione un estado de bidones, con una cantidad de litros como objetivo. 
% Dicho predicado debe ser verdadero cuando se alcanza cierto objetivo (el objetivo debe ser genérico, 
% no necesariamente es 4 litros). Este predicado no requiere ser inversible.

cumpleObjetivo(estado(Objetivo, _), Objetivo).
cumpleObjetivo(estado(_, Objetivo), Objetivo).


% Punto 5.d

% Implementar el predicado resolverBidones/3 que resuelva el problema, teniendo en cuenta que siempre se 
% comienza con los bidones vacíos. 
% Ejemplo:
% ?- resolverBidones(4, 9, Acciones).
% Acciones = [llenarChico, ponerChicoEnGrande, llenarChico, ponerChicoEnGrande, 
% vaciarGrande, ponerChicoEnGrande, llenarChico, ponerChicoEnGrande]
% En el ejemplo, 4 es la cantidad de litros a obtener en cualquiera de los 2 bidones, y 9 es un límite máximo de acciones a realizar (hay 8 en nuestra lista de ejemplo).

% Otro ejemplo válido:
% ?- resolverBidones(3, 1, Acciones)
% Acciones = [llenarChico]
% Este predicado solo requiere ser inversible para el tercer argumento.

resolverBidones(Objetivo, CantidadAcciones, Acciones):-
    resolverBidones(Objetivo, estado(0, 0), CantidadAcciones, Acciones).

resolverBidones(Objetivo, Estado, _, [ ]):-
    cumpleObjetivo(Estado, Objetivo).
resolverBidones(Objetivo, EstadoAnterior, CantidadAcciones, [Accion | Acciones]):- 
    CantidadAcciones > 0, 
    not(cumpleObjetivo(EstadoAnterior, Objetivo)),
    accion(Accion), 
    hacer(Accion, EstadoAnterior, EstadoPosterior), 
    NuevaCantidad is CantidadAcciones - 1,
    resolverBidones(Objetivo, EstadoPosterior, NuevaCantidad, Acciones).
    
    








