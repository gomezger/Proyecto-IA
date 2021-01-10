:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/3,
	    at/2,
	    atPos/2,
	    has/2,
	    entity_descr/2
	  ]).

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado din�mico (creencia)
% manipulado por la actualizaci�n de creencias, para que puedan ser
% consultadon por el resto del c�digo del agente.
%

update_beliefs(Perc):-

%Actualizar los atPos
updateatpos(Perc),
%Actualizar los entity_descr
updateentitydescription(Perc),
%Actualizar los has
updatehas(Perc),
%Actualizar los At
updateat(Perc),
%Actualizar tiempo
retractall(time(_)),
forall(member(time(Timex),Perc),assert(time(Timex))),
%Actualizar la vision
findall(node(Nodex,Nodey,Nodez),member(node(Nodex,Nodey,Nodez),Perc),Nodelist),
forall(member(Nodo,Nodelist),updatevision(Nodo)).


%Actualizacion Entity_descr si la descripcion de una entidad no se conoce, se guarda en los recuerdos, y si cambia la descripcion
%de una entidad ya conocida, se modifica el recuerdo de esta misma a partir de la percepcion.
updateentitydescription(Perc):-
findall(entity_descr(Entityx,Entityy),member(entity_descr(Entityx,Entityy),Perc),Entitylist),
forall(member(entity_descr(Ent1,_Ent2),Entitylist),retractall(entity_descr(Ent1,_))),
forall(member(Entity_descr,Entitylist),assert(Entity_descr)).

 % Actualizacion de Has
updatehas(Perc):-
	forall(member(at(Entity,_X),Perc),retractall(has(_,Entity))),
	findall(has(Hasx,Hasy),member(has(Hasx,Hasy),Perc),Haslist),
	forall(member(has(Ent1,_),Haslist),retractall(has(Ent1,_))),
	forall(member(has(_,Ent2),Haslist),retractall(has(_,Ent2))),
	forall(member(Has,Haslist),assert(Has)).


% Actualizacion de At

updateat(Perc):-
	forall(member(has(_HasEnt,EntHold),Perc),retractall(at(EntHold,_))),
	findall(Nx,member(node(Nx,_Ny,_Nz),Perc),Nodelist),
	forall(member(X,Nodelist),retractall(at(_Y,X))),
	findall(at(Atx,Aty),member(at(Atx,Aty),Perc),Atlist),
	forall(member(at(Ent,At),Atlist),retractall(at(Ent,_))),
	forall(member(At,Atlist),assert(At)).

%Actualizacion de atPos
updateatpos(Perc):-
	forall(member(has(_HasEnt,EntHold),Perc),retractall(at(EntHold,_))),
	findall(Ny,member(node(_Nx,Ny,_Nz),Perc),Nodelist),
	forall(member(X,Nodelist),retractall(atPos(_Y,X))),
	findall(atPos(Atposx,Atposy),member(atPos(Atposx,Atposy),Perc),Atposlist),
	forall(member(atPos(Ent,_EntPos),Atposlist),retractall(atPos(Ent,_))),
	forall(member(Atpos,Atposlist),assert(Atpos)).



updatevision(Nodo):-
		retractall(Nodo),
		assert(Nodo).
