:- module(path_finding,
	  [
	    buscar_plan_desplazamiento/3
	  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% buscar_plan_desplazamiento(+Metas, -Plan, -Destino)
%
buscar_plan_desplazamiento(Metas,PlanMoves,Destino):-
	at([agent,me],Id),
	getHeuristica(Id,Metas,CostoHeuristica),
	busquedaEstrella([[Id,[],0,CostoHeuristica]],Metas,[_NodoActual|Plan],Destino),
	generarplan(Plan,PlanMoves)
	,!.

% a partir de un camino representado por una lista de Ids, genera un plan para recorrerlo.
generarplan([X|[]],[move(X)]):-!.
generarplan([X|Xs],[move(X)|Rs]):- generarplan(Xs,Rs),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%busquedaEstrella(+Frontera,+Metas,-Resultado,-MetaElegida)
%
busquedaEstrella(Frontera, Metas, Resultado,Id):-
			seleccionar(Frontera,[Id,Camino,_CostoG,_CostoH],_RestoFrontera),
			member(Id,Metas),  %Me fijo si ID esta esta en mi lista de metas
			append(Camino,[Id], Resultado). % concateno id y camino.

busquedaEstrella(Frontera,Metas,Resultado,Destino):-
			seleccionar(Frontera,[Id,Camino,CostoG,CostoH],RestoFrontera),
			buscarVecinos([Id,Camino,CostoG,CostoH],Metas,ListaVecinos),
			agregarAFrontera(ListaVecinos,RestoFrontera,NuevaFrontera),
			busquedaEstrella(NuevaFrontera, Metas, Resultado,Destino),
			!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% seleccionar(+Frontera, -Nodo, -RestoFrontera).
% Le pasas Frontera y devuelve un nodo de la frontera y la frontera sin el nodo.
%%%%%%%%%%%%%
seleccionar([Nodo|RestoFrontera], Nodo, RestoFrontera).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agregarAFrontera(+Vecinos, +Frontera, -NuevaFrontera)
%
agregarAFrontera([],Frontera,Frontera):- !.
agregarAFrontera([V|Vecinos],Frontera,NuevaFrontera):-
			agregarAFronteraAux(V,Frontera,FronteraConPrimero),
			agregarAFrontera(Vecinos,FronteraConPrimero,NuevaFrontera),
			!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% agregarAFronteraAux(+Nodo, +Frontera, -NuevaFrontera)
%
% Nodo no pertenece a la frontera, asi que lo agrego.
agregarAFronteraAux(Nodo,[],[Nodo]).

% Nodo ya pertenece a la frontera, y su heuristica es mayor o igual al que estaba en frontera, por lo cual
% la frontera queda igual
agregarAFronteraAux([Id,_Camino,_CostoG, CostoHNuevo], [ [Id, Camino, CostoG, CostoH] | FO ], [ [Id,Camino,CostoG,CostoH] | FO ]):-
		CostoHNuevo >= CostoH,
		!.

% Nodo es distinto al primero nodo de frontera. La heuristica de nodo es mayor o igual a la del primero nodo de frontera ya
% el nodo nuevo ya pertenece al camino del primer nodo de la frontera, por lo cual la frontera queda igual
agregarAFronteraAux([IdNuevo,_Camino,_CostoG, CostoHNuevo], [ [Id, Camino, CostoG, CostoH] | FO ], [ [Id,Camino,CostoG,CostoH] | FO ]):-
		CostoHNuevo >= CostoH,
		member(IdNuevo,Camino),
		!.

% Nodo es distinto al primero nodo de Frontera. La heuristica de de Nodo es menor o igual que la del primer nodo de la
% frontera, por lo cual agregamos a nodo a la nueva frontera eliminando todas las apariciones de nodo de la frontera.
agregarAFronteraAux([IdNuevo,CaminoNuevo,CostoGNuevo,CostoHNuevo], [[Id,Camino,CostoG,CostoH]|FO], [[IdNuevo,CaminoNuevo,CostoGNuevo,CostoHNuevo]|FronteraOrdenada]):-
		CostoH >= CostoHNuevo,
		eliminar(IdNuevo, [[Id,Camino,CostoG,CostoH]|FO], FronteraOrdenada),
		!.

% Caso contrario, dejamos la frontera como esta y analizamos el siguiente nodo de la frontera.
agregarAFronteraAux(Primero,[X|FO],[X|FronteraOrdenada]):-
		agregarAFronteraAux(Primero, FO, FronteraOrdenada),
		!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% eliminar(+Id,+FronteraOriginal,-FronteraOrdenadaSinId)
%
eliminar(_Id,[],[]):- !.
eliminar(Id, [ [Id,_Camino,_CostoG,_CostoH] | FO ], FronteraOrdenada):-
		eliminar(Id, FO, FronteraOrdenada),
		!.
eliminar(Id, [ [Id2,Camino,CostoG,CostoH] | FO ], [ [Id2,Camino,CostoG,CostoH] | FronteraOrdenada ]):-
		%Id\=Id2,
		eliminar(Id,FO,FronteraOrdenada),
		!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%buscarVecinos(+[Id,Camino,CostoG,CostoH],+Metas, -ListaVecinos)
%
buscarVecinos([Id,Camino,CostoG,CostoH], Metas, Resultado):-
			node(Id,_,Vecinos),
			findall([Vec,Cos],  ( member([Vec,Cos], Vecinos), not(member(Vec,Camino)) ), ResultadoAux ),
			crearFrontera([Id,Camino,CostoG,CostoH], ResultadoAux, Metas, Resultado),
			!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% crearFrontera(+[Id,Camino,CostoG,CostoH],+Vecinos, +Metas, -Resultado)
%
crearFrontera(_Primero,[], _Metas, []):- !.
crearFrontera([Id, Camino, CostoG,CostoH], [[Vec,Cos]|Vecinos], Metas, [[Vec,CaminoAux,CostoGAux,CostoHAux]|ResultadoAux]):-
			append(Camino,[Id],CaminoAux),
			getHeuristica(Vec,Metas,CostoHNuevo),
			CostoGAux is Cos+CostoG,
			CostoHAux is CostoGAux+CostoHNuevo,
			crearFrontera([Id,Camino,CostoG,CostoH],Vecinos,Metas, ResultadoAux).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getHeuristica(+IdMe,+Metas,-Costo)
% Calcula la distancia a la meta más cercana usando el predicado distancia/3 definido en extras_for_agents.
%
getHeuristica(Id,Metas,0):- member(Id,Metas),!.
getHeuristica(Id,[Idmeta],Dist):- node(Id,VMe,_),
									node(Idmeta,Vmeta,_),
									distance(VMe,Vmeta,Dist),
									!.
getHeuristica(Id,[Idmeta|Nodos],Dist):-	node(Id,VMe,_),
											node(Idmeta,Vmeta,_),
											distance(VMe,Vmeta,DistAux),
											getHeuristica(Id,Nodos,DistMin),
											Dist is min(DistAux,DistMin),
											!.
