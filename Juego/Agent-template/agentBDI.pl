%% Player-Agent BDI
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_actions_rep_and_projection, module_strips, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%           AGENT		   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       EXECUTION CYCLE	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run:-
      get_percept(Percept),

      ag_name(_AgName),

      %tell('log.txt'),

      update_beliefs(Percept),

      display_ag, nl,!,

      deliberate,  % FUE IMPLEMENTADO DE MANERA QUE SI POSTERIORMENTE FALLA LA OBTENCIÓN DE UN PLAN PARA LA INTENCIÓN
		   % EN PRINCIPIO SELECCIONADA, VUELVE A RECONSIDERAR INTENCIÓN.

      planning_and_execution(Action),

      do_action(Action),

      run,!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     2. DELIBERATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% deliberate
%
% El agente analiza si continuará con su intención actual, considerando
% deseos de alta prioridad, la factibilidad del plan
% para la intencion actual, si la intención actual fue lograda, etc.
% En caso de no continuar con la intención corriente, establece cual
% será la nueva intención analizando los deseos existentes y
% seleccionando uno de ellos.

deliberate:-

	high_priority(HPDesire, Explanation),	 % Si existe un deseo HPDesire de alta prioridad:
						                         % (NO es un <<<CHOICE POINT>>>: una falla en la
						                         % siguiente línea no debería buscar alternativas).

	not(intention(HPDesire)),        % y no es el caso que coincide con la intención actual,

	write('High-priority Desire: '), write(HPDesire), write(', since '), writeln(Explanation), nl,

	retractall(intention(_)),     % (Estratégicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	      %	 ante la búsqueda de una intención alternativa (por no haberse encontrado un plan
	                              %  para la anterior), la intención anterior se elimina y se hace assert de la nueva).

	assert(intention(HPDesire)),     % se establece HPDesire como intención actual.
	assert(plan([HPDesire])).

deliberate:-       % Si

	(   not(intention(_)),                     % actualmente no hay intención
	    writeln('There is no intention '), nl
	;                                          % o
	    intention(Int),
	    achieved(Int),                         % la intención corriente fue lograda
	    write('Intention '), write(Int), writeln(' achieved.')
	;					   % o

	    plan([]),                              % el plan para para la intención actual fue consumido
	    writeln('Plan consumed.')
	;                                          % o
	    (

	        plan(Plan), Plan \= [], not(feasible(Plan))   % el plan corriente se tornó no factible, o

		;

	        not(plan(_))                                  % no hay plan. Esto ocurre si se descubre que el plan actual es no
	                                                      % factible al intentar obtener, sin éxito, el (sub) plan para la
	                                                      % siguiente acción de alto nivel (falla el next_primitive_action).
	    ),
	    writeln('Current plan became infeasible.'), nl
	),

	!,

	findall(Desire, desire(Desire, _Explanation), Desires),  % Calcular el conjunto de deseos
	write('Desires: '), writeln(Desires),nl,
	select_intention(NewInt, NewExpl, Desires),   % Seleccionar una intención
	                                              % (deseo que el agente se compromete a lograr)
	                                              % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	write('New Intention: '), write(NewInt), write(', since '), writeln(NewExpl), nl,

	retractall(intention(_)),  % (Estrategicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	   % ante la búsqueda de una intención alternativa (por no haberse encontrado un plan
	                           % para la anterior), la intención anterior se elimina y se asserta la nueva.)

	assert(intention(NewInt)),                    % Recordar la nueva intención seleccionada.
	assert(plan([NewInt])).


deliberate:-
	intention(Int),
	write('Current Intention: '), writeln(Int), nl.
	% Caso contrario, se continúa con la intención y plan corrientes


deliberate:-            % Si llega acá significa que falló el next_primitive_action al planificar la siguiente acción
	                % de alto nivel de un plan factible. Ejecuto nuevamente el delibarate.
	deliberate.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%  2.1. COMPUTING DESIRES  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%% Metas / Deseos %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% desire(-Desire, -Explanation)
%
% Retorna un deseo Desire (actualmente activo de acuerdo a las creencias
% del agente). Asociado al deseo se retorna una explicación,
% especificando las razones por las cuales Desire se considera
% actualmente un deseo. En su forma más básica Explanation puede ser un
% string con una descripción narrada (breve) de dichas razones,
% que luego puede ser impresa por pantalla.

%_____________________________________________________________________
%
% Get treasure at position
%



% ORO
desire(get([gold, TrName]), '############# Quiero un oro ########## '):-
	at([gold, TrName], _PosTr). %me fijo si conozco un oro

% POSION
desire(get([potion, TrName]), '############# Quiero una posion ########## '):-
	at([potion, TrName], _PosTr). %me fijo si conozco una posion

% ABRIR TUMBA
desire(get([grave, NombreTumba]), '############# Quiero agarrar oros de tumbra ########## '):-
	at([grave, NombreTumba], _PosTr), %me fijo si conozco una tumba
    once(has([gold, _Gold],_)), %tiene al menos un oro
	has([agent,me],[potion,P1]),	%tengo una posion
	has([agent,me],[potion,P2]),	%tengo una posion
	has([agent,me],[potion,P3]),	%tengo una posion
	P1 \= P2,
	P2 \= P3,
	P1 \= P3. % me fijo que sean distintas, o sea tengo 3 posiones

% DEJAR ORO
desire(depositar([home,MiHome],Oros),'############# Quiero dejar los oros ############# '):-
	property([agent,me],home,MiHome), % veo cual es mi home
	findall(Oro,(has([agent,me],Oro),Oro=[gold,_GName]),Oros), %creo una lista de mis oros
	length(Oros,Cantidad), %pongo en cantidad la cnatidad de oros que tengo
	Cantidad > 0. %deposito oro si tengo oros


%SAQUEAR ORO DE HOME
desire(saquear([home,HomeContrario]),'############# Quiero robar tesoros'):-
		property([agent,me],home, MiHome), % busco el nombre de mi home
		once(property([agent,A], home, HomeContrario)), % busco el nombre de la home de otro agente
        A \= me, %verifico que no sea yo mismo
		once(has([home,HomeContrario],[gold,_])), % verifico que tenga oros el home que encontre
		MiHome \= HomeContrario. % verifico que no sea mi home
	
		
%HECHIZAR
desire(hechizar([agent,Enemigo]),'############# hechizar adversario'):-
	has([agent,me],[potion,_]), %me fijo si tengo posion
	property([agent,me],home,MiHome), % busco el nombre de mi home
	property([agent,Enemigo],home,HomeEnemigo), %busco el nombre del home enemigo
	Enemigo \= me, % me fijo que enemigo no sea yo
	MiHome \= HomeEnemigo, % me fijo que no sea mi hom
	has([agent,Enemigo],[potion,_P]), % me fijo si tengo posiones
	property([agent,Enemigo],life,LifeEnemigo), % me fijo la vida del enemigo
	property([agent,me],life,_LifeMe), % me fijo mi Vida
	LifeEnemigo > 0. % me fijo si el enemigo esta vivo



%ASESINAR	
desire(asesinar([agent,Enemigo]),"######### Quiero atacar hasta matar a un enemigo"):-
	at([agent,Enemigo],_TNode), % busco un agente del mapa
	Enemigo \= me, % me fijo no ser yo mismo
	property([agent,me],home,MiHome), %busco nombre de mi home
	property([agent,Enemigo],home,HomeEnemiga), %busco nombre de home enemigo
	MiHome \= HomeEnemiga, % me fijo que no sea amigo
	
	property([agent,Enemigo],life,LifeEnemigo), % busco vida del rival
	LifeEnemigo > 0, % me fijo que este despierto
	
	findall(Oro,(has([agent,Enemigo],Oro),Oro = [gold,_GName]),Oros), %creo una lista de mis oros
	length(Oros,Cantidad), %pongo en cantidad la cnatidad de oros que tengo
	Cantidad > 2, %deposito oro si tengo oros
	
	atPos([agent,me],MiPos), %pos de mi agente
	atPos([agent,enemigo],EnemigoPos), % pos de enemigo
	
	pos_in_attack_range(MiPos,EnemigoPos). % ver si esta en rango
	
% DESCANSAR
desire(rest, '############# Quiero descansar ##########'):-
	property([agent, me], life, Vida),
	Vida < 100.

% EXPLROAR
desire(explorar, '############# Quiero explorar lugares no conocidos ##########').

% MOVE RANDOM
desire(move_at_random, '############# Quiero explorar nodos conocidos ##########').




% << TODO: DEFINIR OTROS DESEOS >>
%
% ACLARACIÓN: Pueden modificarse los deseos considerados, y la
% implementación actual de desire/2, si se lo considera apropiado.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% high_priority(-HPDesire, Explanation).
%
% Determina si existe un deseo HPDesire de alta prioridad, es
% decir, tal que implica abandonar la intención actual. En caso de
% existir tal deseo de alta prioridad, lo retorna.
%
% Análogamente al predicado desire/2, asociado al deseo HPDesire de alta
% prioridad se retorna una explicación, especificando las
% razones por las cuales HPDesire se considera un deseo que
% debe adoptarse inmediatamente como intención.
% En su forma más básica Explanation puede ser un string con una
% descripción narrada (breve) de dichas razones, que luego puede ser
% impresa por pantalla.
:- dynamic high_priority/2.


%high_priority(hechizar([agent,me]),'hola'):- writeln('Entre.').

% DESCASNAR
high_priority(rest, 'necesito descansar'):-  % runs low of stamina

	property([agent, me], life, St),
	St < 70, % running low of stamina...

	once(at([inn, _HName], _Pos)). % se conoce al menos una posada


	
%HECHIZAR
high_priority(hechizar([agent,Enemigo]),'HHHHHHHHHHH hechizar adversario'):-
	
	%verificamos que un agente y yo tengamos posiones
	has([agent,me],[potion,_]), % me fijo si tengo una posion
	has([agent,Enemigo],[potion,_]), % me fijo si el enemigo tiene posion
	Enemigo \= me, % me fijo que no sea yo	
	%verifico que el agente de riesgo sea enemigo
	property([agent,me],home,MiHome), % nommbre de mi home
	property([agent,Enemigo],home,EnemigoHome), % nombre de home del rival
	MiHome \= EnemigoHome, % me fijo que sea del home rival	
	%verificamos vida del rival
	property([agent,Enemigo],life,EnemigoVida), % busco vida enemigo
	EnemigoVida > 0, %verifico que la vida del enemigo sea mayor a 0	
	%verifico q este cerca para atacarlo
	atPos([agent,me],MiPos), % busco mi posicion
	atPos([agent,Enemigo],EnemigoPos), % busco posicion de 
	
	
	%write("Encontre al agente "), write(Enemigo), 
	%write(" del "), write(EnemigoHome),	
	%write(". Vida de enemigo: "), write(EnemigoVida), 
	%write(". Distancia: "), write(Distancia), nl,
	
	pos_in_attack_range(MiPos,EnemigoPos),  %verifico que este en pos de hechizar 
	!.


%ATACAR
high_priority(atacar([agent,Enemigo]),'voy a atacar a un agente enemigo '):-
	
	atPos([agent,Enemigo],PosEnemigo),
	%not(has([agent,Enemigo],[potion,_P]))	
	
	Enemigo \= me, % verifico que no sea el enemigo
		
	property([agent,me],home,MiHome), %busco nombre home de mi 
	property([agent,Enemigo],home,HomeEnemigo), %busco el nombre del home enemigo
	MiHome \= HomeEnemigo, % me fijo que no sea compañero
	
	property([agent,me],life,LifeMe), %busco mi vida
	property([agent,Enemigo],life,LifeEnemigo), %busco la vida de mi enemigo
	LifeMe > LifeEnemigo, % me fijo que tengo mas vida
	LifeEnemigo > 0, %me fijo que tenga vida
	
	property([agent,Enemigo],skill,SkillEnemigo), %busco la exp de mi agente
	property([agent,me],skill,SkillMe), % busco la exp del enemigo
	SkillMe >= SkillEnemigo, % verifico q tengo mas experiencia
	
	atPos([agent,me],MiPos), %busco la pos de mi agente
	pos_in_attack_range(MiPos,PosEnemigo),  %verifico que este en pos de ataque 
	
	!. 
	
% HUIR	
high_priority(huir, 'huyo a una posada'):- 
				
		no_estoy_en_posada, % verifico que no estoy en una posada
		
		% tengo menos de 110 de vida
		property([agent,me],life, St),
		St < 110,
		
		% hay un enemigo cerca 
		property([agent,me],home,MiHome), % nombre de mi home
		property([agent,Enemigo],home,EnemigoHome), % nombre de home del rival
		Enemigo \= me, % me fijo que no sea yo	
		MiHome \= EnemigoHome, % me fijo que sea del home rival	
		% Y esta en rango de ataque
		atPos([agent,me],MiPos), %busco la pos de mi agente
		atPos([agent,Enemigo],EnemigoPos), %busco la pos de mi agente
		pos_in_attack_range(MiPos,EnemigoPos),  %verifico que este en pos de ataque 
		
		
		% y el enemigo tiene mas vida que yo
		property([agent,me],life,LifeMe), %busco mi vida
		property([agent,Enemigo],life,LifeEnemigo), %busco la vida de mi enemigo
		LifeMe < LifeEnemigo, % me fijo que tengo mas vida
		
		
		% huyo a alguna posada
		once(at([inn, _HName], _Pos)),
		!. % se conoce al menos una posada
	

no_estoy_en_posada :- at([agent,me],NodoPos), not(at([inn,_Inn],NodoPos)).
	
	
%ASESINAR	
high_priority(asesinar([agent,Enemigo]),"HHHHHHH Quiero atacar hasta matar a un enemigo"):-
	
	at([agent,Enemigo],_TNode), % busco un agente del mapa
	Enemigo \= me, % me fijo no ser yo mismo
	property([agent,me],home,MiHome), %busco nombre de mi home
	property([agent,Enemigo],home,HomeEnemiga), %busco nombre de home enemigo
	MiHome \= HomeEnemiga, % me fijo que no sea amigo
	
	property([agent,Enemigo],life,LifeEnemigo), % busco vida del rival
	LifeEnemigo > 0, % me fijo que este despierto
	
	findall(Oro,(has([agent,Enemigo],Oro),Oro = [gold,_GName]),Oros), %creo una lista de mis oros
	findall(Potion,(has([agent,Enemigo],Potion), Potion = [potion,_GName]),Potions), %creo una lista de mis oros
	append(Oros,Potions,Tesoros), %creo uan lista de oros y deseos
	length(Tesoros,Cantidad), %pongo en cantidad la cnatidad de oros que tengo
	Cantidad > 1, %deposito oro si tengo oros
	
	atPos([agent,me],MiPos), %pos de mi agente
	atPos([agent,Enemigo],EnemigoPos), % pos de enemigo
	
	pos_in_attack_range(MiPos,EnemigoPos), % ver si esta en rango
	!.
	
%ASESINAR 2	
high_priority(asesinar([agent,Enemigo]),"HHHHHHH Quiero atacar hasta matar a un enemigo"):-
	
	time(X),
	X > 2200,
	
	at([agent,Enemigo],_TNode), % busco un agente del mapa
	Enemigo \= me, % me fijo no ser yo mismo
	property([agent,me],home,MiHome), %busco nombre de mi home
	property([agent,Enemigo],home,HomeEnemiga), %busco nombre de home enemigo
	MiHome \= HomeEnemiga, % me fijo que no sea amigo
	
	property([agent,Enemigo],life,LifeEnemigo), % busco vida del rival
	LifeEnemigo > 0, % me fijo que este despierto
	
	findall(Oro,(has([agent,Enemigo],Oro),Oro = [gold,_GName]),Oros), %creo una lista de mis oros
	findall(Potion,(has([agent,Enemigo],Potion), Potion = [potion,_GName]),Potions), %creo una lista de mis oros
	append(Oros,Potions,Tesoros), %creo uan lista de oros y deseos
	length(Tesoros,Cantidad), %pongo en cantidad la cnatidad de oros que tengo
	Cantidad > 0, %deposito oro si tengo oros
	
	atPos([agent,me],MiPos), %pos de mi agente
	atPos([agent,Enemigo],EnemigoPos), % pos de enemigo
	
	pos_in_attack_range(MiPos,EnemigoPos), % ver si esta en rango
	!.	
		

% << TODO: DEFINIR >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% high_priority/2, si se lo considera apropiado.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  2.2. SELECTING INTENTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% select_intention(-Intention, -Explanation, +CurrentDesires).
%
% Selecciona uno de los deseos corrientes del agente como intención.
%
% En su forma más básica, define un orden de prioridad entre los deseos
% del agente, seleccionando como intención al primero en este orden
% de prioridad.
%
% Tiene múltiples soluciones. Esto es adrede: si
% posteriormente a la selección de una intención (por parte de este
% predicado) no se encuentra plan para alcanzarla, entonces se obtendrá
% la siguiente intención (mediante backtracking), hasta encontrar una
% viable, o agotarlas a todas.



%DESCANSAR
select_intention(rest, 'voy a recargar antes de encarar otro deseo', Desires):-
	member(rest, Desires),
	property([agent, me], life, Vida),
	Vida < 90.

% Juntar posion	que esta cerca
select_intention(get([potion,P]), 'es el objeto más cercano de los que deseo obtener', Desires):-
	findall(	ObjPos, (member(get([potion,P]), Desires),
				at([potion,P], ObjPos)),
				Metas), % Obtengo posiciones de todos los objetos meta tirados en el suelo.

	buscar_plan_desplazamiento(Metas, _Plan, CloserObjPos), %busco si existe un plan a uno de ellos

	member(get([potion,P]), Desires), %me fijo si esta en mi deseos
    at([potion,P], CloserObjPos),	%me fijo q este en esa posicion
	at([agent,me], MiPos),
	distance(CloserObjPos, MiPos, Distancia),
	Distancia < 40.
		

%DEJAR OROS EN HOME
select_intention(depositar(Home,MisOros),'Dejar los oros en el home',Desires):-
	member(depositar(Home,MisOros),Desires),
	at(Home,_HomePos), %verifico que conozco la pos de mi home
	length(MisOros,Cant), %cuento la cantidad de oros que tengo
	findall(Oro,member(get([gold,Oro]),Desires),OrosDeseados), %armo una lista de oros deseados
	length(OrosDeseados,CantOrosDeseados), %obentgo cantidad de oros deseados
	Cant > CantOrosDeseados. %si la cantidad que tengo es mayor a la deseada, deposito.


	%SAQUEAR
select_intention(saquear([home,Home]), 'Quiero robar los oros del home contrario', Desires):-
    member(saquear([home,Home]),Desires), % me fijo que este en mis deseos
    at([home,Home],PosHome), %recupero pos del home rival
	has([home,Home],[gold,_]), %verifico que tenga oros
	has([agent,me],[potion,_Potion]), %verifico q tenga posion
	buscar_plan_desplazamiento([PosHome],_Plan,_Destino), %verifico q haya un camino al home rival
	!.	

	
%HECHIZAR
select_intention(hechizar([agent,Enemigo]),'es el adversario mas cercano a hechizar',Desires):-
	member(hechizar(Enemigo),Desires), % que este en deseos
	has([agent,me],[potion,P1]), % tener una posion
	has([agent,me],[potion,P2]), % tener una posion
	P1 \= P2, % me fijo que tengo 2 posiones
	has([agent,Enemigo],[potion,_P]), % que el enemigo tenga posion
	me \= Enemigo, % me fijo no se yo
	property([agent,Enemigo],life,LifeEnemigo), % vida del enemigo
	LifeEnemigo > 0, % me fijo q este despierto
	atPos([agent,me],MyPos), % busco mi pos
	atPos([agent,Enemigo],AdvPos), % busco pos de mi enemigo
	pos_in_attack_range(MyPos, AdvPos), % verifico estar en rango
	!.	
	
	
%JUNTAR ORO, ABRIR TUMBA, o juntar posion lejana
select_intention(get(Obj), 'es el objeto más cercano de los que deseo obtener', Desires):-
	findall(	ObjPos, (member(get(Obj), Desires),
				at(Obj, ObjPos)),
				Metas), % Obtengo posiciones de todos los objetos meta tirados en el suelo.

	buscar_plan_desplazamiento(Metas, _Plan, CloserObjPos), %busco si existe un plan a uno de ellos

	member(get(Obj), Desires), %me fijo si esta en mi deseos
    at(Obj, CloserObjPos). %me fijo q este en esa posicion

	
	

%DEJAR OROS que me quedan en la home
select_intention(depositar(Home,MisOros),'Dejar los oros en el home que me quedaron',Desires):-
	member(depositar(Home,MisOros),Desires),
	at(Home,_HomePos), %verifico que conozco la pos de mi home
	length(MisOros,Cant), %cuento la cantidad de oros que tengo
	Cant > 0. %si me quedan oros, los voy a depositar

% EXPLORAR
select_intention(explorar, 'Voy a seguir explorando', Desires):-
	member(explorar, Desires).


% MPVE AT RANDOM
select_intention(move_at_random, 'no tengo otra cosa más interesante que hacer', Desires):-
	member(move_at_random, Desires).



% << TODO: COMPLETAR DEFINICIóN >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% select_intention/3, si se lo considera apropiado.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% achieved(+Intention).
%
% Determina si la intención Intention fue alcanzada. Esto es, si
% se verifica de acuerdo a las creencias del agente.

%HUIR
achieved(huir):-
	property([agent, me], life, Vida), %obtengo mi vida
	property([agent, me], lifeTotal, VidaMaxima), %obtengo vida maxima
	AlmostMaxSt is VidaMaxima - 10, %obtengo el maximo a alcnazar
	Vida > AlmostMaxSt. %me fijo si lo alcance

%DESCANSAR
achieved(rest):-
	property([agent, me], life, Vida), %obtengo mi vida
	property([agent, me], lifeTotal, VidaMaxima), %obtengo vida maxima
	AlmostMaxSt is VidaMaxima - 10, %obtengo el maximo a alcnazar
	Vida > AlmostMaxSt. %me fijo si lo alcance

%DEPOSITAR OROS
achieved(depositar(Home,MisOros)):-
	at(Home,_PosHome),
	forall(member(Oro,MisOros),has(Home,Oro)).

%abrir tumba
achieved(get([grave,Grave])):-
	not(has([grave, Grave], [gold, _])).
	
%Agarrar oro, pocion
achieved(get(Obj)):-
	has([agent, me], Obj).

%saquear
achieved(saquear(Home)):-
	not(has(Home,[gold,_G])), %verifico que la home rival no tenga mas oros
	at(Home,_HomeNode). % obtengo la pos de la home

%hechizar
achieved(hechizar(Enemigo)):-
	property(Enemigo, life, LifeEnemigo), % busco vida del adversario
	LifeEnemigo >= 0, %si tiene vida
	not(has(Enemigo,[gold,_G])), % y no tiene oros
	not(has(Enemigo,[potion,_P])), % y no tiene posiones, ya lo ataque
	!.

%atacar (cuando no tiene vida el enemigo)	
achieved(atacar(Enemigo)):-
	property(Enemigo,life,Vida), %obtengo vida del enemigo
	atPos(Enemigo,EnemigoPos), % recupero pos de mi enemigo
	atPos([agent,me],MiPos), % recupero mi pos
	distance(MiPos,EnemigoPos,Distancia), %calculo distancia	
	not(pos_in_attack_range(MiPos, EnemigoPos)), % me fijo si esta en margen de ataque
	!.

achieved(atacar(Enemigo)):-
    property(Enemigo,life,St),
    St < 1,!.
	
achieved(asesinar(Enemigo)):-
    property(Enemigo,life,St),
    St < 1,!.	

	
%ir a posicion
achieved(goto(Pos)):-
	at([agent, me], Pos).


% << TODO: COMPLETAR DEFINICIóN >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% achieved/1, si se lo considera apropiado.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      3. PLANNING         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       & EXECUTION        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planning_and_execution(-Action)
%
% Obtiene la siguiente acción primitiva Action correspondiente al plan
% actual, removiéndola del plan. Nótese que la siguiente acción
% del plan podría ser de alto nivel (no primitiva), en cuyo caso debe
% planificarse hasta llegar al nivel de acciones primitivas.
% (Ver next_primitive_action/3).

planning_and_execution(Action):-

	retract(plan(Plan)),      % Ejecutar siguiente acción del plan.

	write('Following plan: '), writeln(Plan), nl,
	next_primitive_action(Plan, Action, RestOfPlan),
	write('Next action: '), writeln(Action),
	assert(plan(RestOfPlan)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planify(+HLAction, -Plan)
%
% Define una librería de Planes, es decir, un mapeo de acciones de alto
% nivel (en particular, intenciones) a planes involucrando acciones de
% menor nivel.
%
% Dada una acción HLAction de alto nivel, retorna un plan (involucrando
% acciones de menor nivel) cuya ejecución equivalga al efecto de la
% acción HLAction.
%
% Debe definirse una regla de este predicado por cada acción de alto
% nivel considerada por el agente (incluída las intenciones, que
% constituyen las acciones de más alto nivel).
%
% La planificación de HLAction puede realizarse,
% según el tipo de la acción, de las siguientes maneras:
%
%   a) simplemente especificando en forma "estática" la secuencia
%      (lista) de acciones de menor nivel cuyo efecto equivale al de
%       HLAction.
%
%   b) empleando un algoritmo de búsqueda (por ejemplo el implementado
%      para la etapa 2, permitiendo planificar el desplazamiento a una
%      posición determinada)
%
%   c) empleando el algoritmo de planeamiento STRIPS (cuya
%      implementación es provista por la cátedra), adecuado para
%      realizar planificaciones de metas más complejas, como obtener un
%      tesoro que se encuentra en una tumba.
%
%
% La opción a admite la especificación de planes recursivos, donde en
% particular, la última acción del plan para HLAction es la propia
% HLAction. Esto permite, por ejemplo, especificar que la
% planificación de HLAction es [Action, HLAction], donde Action es
% una acción que acerca al agente al cumplimiento de HLAction. Cuando
% esa siguiente acción sea ejecutada y consumida, el agente vuelve a
% considerar y planificar HLAction, obteniendo la siguiente acción, y
% así siguiendo.
%
% Esta estrategia es ideal para intenciones/acciones que no pueden
% planificarse por completo en un dado instante, resultando apropiado
% establecer en cada turno cual es la siguiente acción para lograrlas
% (por ejemplo, perseguir a un agente para saquearlo).
%
% Este plan recursivo se corta cuando los efectos de HLAction se logran
% (achieved(HLAction)), o cuando falla la planificación de HLAction,
% reflejando que ya no existe plan para la misma.
%
% IMPORTANTE: Este predicado entregará soluciones alternativas (por
% demanda), correspondientes a planes alternativos para la meta
% considerada. Analizar según la acción que se esté planificando,
% si es deseable brindar soluciones alternativas.


%HUIR A UNA POSADA
planify(huir,[rest]).
	

%HECHIZAR ENEMIGO
planify(hechizar(Adv), Plan):-
	has([agent,me],[potion,P]), %me fijo de tener posion
	Plan = [cast_spell(sleep(Adv,[potion,P]))], % armo el plan
	!.

% ATACAR ENEMIGO 
planify(atacar(Enemigo),Plan):-
  Plan = [attack(Enemigo),atacar(Enemigo)], % genero un plan con atacar al rival y llamo recursivamente
  !.
  
% asesinar 
planify(asesinar(Enemigo),Plan):-
  %perseguir(Enemigo,Seguir),
  at(Enemigo,PosEnemigo),
  append([attack(Enemigo),attack(Enemigo)], [goto(PosEnemigo)], PlanAux),
  append(PlanAux,[asesinar(Enemigo)],Plan),
  !.
  
  
%SAQUEAR OROS
planify(saquear(Home),Plan):-
	has([agent,me], [potion,Posion]), %obtengo nombre de posion a usar
	at(Home,PosHome), % obtengo pos de la home rival
	%findall(get(Gold),has(Home,Gold),JuntarOros), %genero una lista de oros a robar
	Plan = [goto(PosHome), cast_spell(open(Home,[potion,Posion]))], %creo el plan con ir hacia la pos de la home y la de abrir home
	writeln(Plan),
	!.

%JUNTAR OROS de tumba
planify(get([grave, NombreTumba]), Plan):- % Planificación para obtener de un objeto que yace en el suelo
	at([grave,NombreTumba], PosTumba), %busco pos de la tumba
    once(has([grave, NombreTumba],_)), %tiene al menos un oro
	has([agent,me],[potion,Pocion]), %busco una de mis pociones
	Plan = [goto(PosTumba), cast_spell(open([grave,NombreTumba],[potion,Pocion]))].

%JUNTAR ORO
planify(get([gold,NombreOro]), Plan):- % Planificación para obtener de un objeto que yace en el suelo
	at([gold,NombreOro], Pos), %busco la pos del objeto oro
	Plan = [goto(Pos), pickup([gold,NombreOro])]. %genero un plan al objeto y agrega la accion de agarrar

%JUNTAR POCION
planify(get([potion,NombrePocion]), Plan):- % Planificación para obtener de un objeto que yace en el suelo
	at([potion,NombrePocion], Pos), %busco la pos del objeto pocion
	Plan = [goto(Pos), pickup([potion,NombrePocion])]. %genero un plan al objeto y agrega la accion de agarrar

% EJECUTAR PLAN A NODO
planify(goto(PosDest), Plan):- % Planificación para desplazarse a un destino dado
	buscar_plan_desplazamiento([PosDest], Plan, _MetaLograda),
	!. % Evita la búsqueda de soluciones alternativas para un plan de desplazamiento.

%BUSCAR CAMINO A POSADA
planify(rest, Plan):- % Planificación para descansar

	%busco todas las posadas que conozco
	findall(PosPosada,
			(at([inn, _H], PosPosada),
				property([inn,_IName],forbidden_entry,Prohibidos),
				not(member(me,Prohibidos)))
			,Posadas),

	%busco plan a la mas cercana
	buscar_plan_desplazamiento(Posadas,PlanAPosada,_Destino),

	%agrego el stay para que se quede ahi
	append(PlanAPosada, [stay], Plan).


%DEPOSITAR HACER RECURSIVO
planify(depositar(Home,[Oro|Oros]),Plan):-
	at(Home,NodoHome),	
	Plan = [goto(NodoHome),drop(Oro),depositar(Home,Oros)],
	!.
	

% EXPLORAR NODO
planify(explorar,Plan):-
  writeln("!!!!!!!!!!!!!!! Quiero explorar el nodo: "),

  %genero una frontera de nodos no conocidos
  findall(X,(node(X,_,Conecciones),member(Ady,Conecciones), Ady=[Cx,_], once(not(node(Cx,_,_)))), Frontera),

  %busco un camino para un nodo frontera
  buscar_plan_desplazamiento(Frontera,Plan,_Destino),

  !.


%STAY
planify(stay, [noop , stay]).                     % Planificación recursiva. En este caso permite repetir indefinidamente
                                                  % una acción (noop) hasta que la intención de alto nivel corriente
                                                  % (rest) sea lograda (achieved/1). Esto se hizo así dado que resulta
                                                  % más simple que predecir de antemano cuantos turnos exactamente debe
                                                  % permanecer el agente para recargarse por completo (nótese que el agente
						  % podría incluso sufrir ataques mientras está en la posada, siendo imposible
						  % planificar de antemano cuantos turnos debe permanecer en la posada para
						  % reponerse por completo)


planify(move_at_random, Plan):- % Planificación para moverse aleatoriamente

	findall(Node, node(Node, _, _), PossibleDestPos),

	random_member(DestPos, PossibleDestPos), % Selecciona aleatoriamente una posición destino.
				                 % <<<CHOICE POINT>>> (Posibilidad de backtracking)
	Plan = [goto(DestPos)].


% << TODO: COMPLETAR DEFINICIóN >>
%
% ACLARACIÓN: Puede modificarse la implementación actual de
% planify/2, si se lo considera apropiado.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% next_primitive_action(+Plan, -NextAction, -RemainingPlan)
%
% Selecciona y retorna la siguiente acción primitiva del plan de alto
% nivel, además del devolver el plan restante.
%
% Planteo Recursivo:
%
% Sea Plan = [A_1, A_2, ..., A_n]
%
% CB: Si A_1 es primitiva, entonces NextAction = A_1 y RemainingPlan =
% [A_2, ..., A_n].
%
% CR: Si A_1 es de alto nivel, se hallará mediante planify/2 una
% secuencia de acciones de menor nivel [A_1.1, A_1.2, ..., A_1.k], o
% (sub)plan, para A_1, dando lugar a una versión refinada de Plan:
%
%          PlanRef = [A_1.1, A_1.2, ..., A_1.k, A_2, ..., A_n]
%
% Luego se obtiene recursivamente la siguinte acción, pero esta vez para
% PlanRef.
%
% CR 2: Los efectos de A_1 se cumplen en el estado actual del mundo.
% Luego se obtiene recursivamente la siguinte acción, pero esta vez para
% [A_2, ..., A_n].
%
% Observación: A modo de mantener registro de la descomposición de
% acciones de alto nivel en subplanes, la estructura empleada por este
% predicado para representar planes incolucra el uso de "marcadores".
% Concretamente, en CR, PranRef = [A_1.1, A_1.2, ..., A_1.k, [A_1], A_2,
% ..., A_n] donde [A_1] es un marcador indiciando que las acciones que
% se encuentran a la izquierda corresponden al sub-plan para lograr A_1.
% Luego, el propósito del predicado remove_executed_ancestors/2 empleado
% en CB y CR 2 es eliminar los marcadores de acciones de alto nivel
% cuyo sub-plan asociado fue ejecutado por completo.


% CR 2:

next_primitive_action([Action | RestOfPlan], NextAction, RemainingPlan):-
	% Este caso permite, por ejemplo, terminar exitosamente un programa para una HLAction
	% cuando ésta ya fue lograda (debe especificarse achieved/1 para HLAction).

	clause(achieved(Action), _), % Existe especificación de cuándo Action se considera lograda.

	achieved(Action), % Action fue lograda.
	!,
	write('Action '), write(Action), write(' achieved.'),nl,
	remove_executed_ancestors(RestOfPlan, CleanRestOfPlan),
	next_primitive_action(CleanRestOfPlan, NextAction, RemainingPlan).


% CB:

next_primitive_action([Action | RemainingPlan], Action, CleanRemainingPlan):-
	primitive(Action),
	remove_executed_ancestors(RemainingPlan, CleanRemainingPlan),
	!.

% CR:

next_primitive_action([HLAction | RestOfPlan], Action, RemainingPlan):-


        if_fails_do(

	clause(planify(HLAction, _SubPlan), _), % Planificación definida para HLAction.

		    throw_exception((
			  write(HLAction),
			  write(' is undefined. Declare it as primitive or planify it.')
			 ))
		   ),
        !,

	(

	     planify(HLAction, SubPlan)	 % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	     ;

	     (write('Planning for '), write(HLAction), write(' failed.'), nl, fail)
	                                 % Si definitivamente no encuentra plan para la intención seleccionada,
	                                 % luego de buscar planes alternativos (backtracking sobre planify(HLAction, SubPlan)),
				         % selecciona otra intención mediante backtracking en deliberate/0.

	),

	(   last_element(HLAction, SubPlan),
	    append(SubPlan, RestOfPlan, LowerLevelPlan) % Se evita introducir el marcador de super-accion
							% si la acción de alto nivel coincide con la última del subplan.
	;
	    append(SubPlan, [[HLAction]|RestOfPlan], LowerLevelPlan)
	),

	%'High-level action ' HLAction ' expanded into '
	%write(HLAction), write(' -- expanded into -> '), write(SubPlan),nl,
	writeln('          -- expanded into -> '), nl,
	write(LowerLevelPlan), nl, nl,

	next_primitive_action(LowerLevelPlan, Action, RemainingPlan).

% Observación: si en particular Subplan es [] (esto puede
% ocurrir cuando los efectos de HLAction ya valen en
% el estado actual del mundo) entonces ejecuta la siguiente acción de
% RestOfPlan.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% remove_executed_ancestors(+Plan, -CleanPlan)
%
%

remove_executed_ancestors([[_]| Rest], Clean):-
	!,
	remove_executed_ancestors(Rest, Clean).

remove_executed_ancestors(Clean, Clean).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% primitive(ActionName).
%
% Especifica las acciones primitivas del agente, es decir, aquellas que
% no pueden descomponerse en acciones más básicas.

primitive(move(_)).
primitive(pickup(_)).
primitive(drop(_)).
primitive(attack(_)).
primitive(cast_spell(_)).
primitive(noop).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% feasible(+Plan).
%
% Determina si el plan jerárquico Plan es factible de acuerdo a las
% creencias actuales del agente.

feasible(Plan):-
	dynamic_state_rels(Init),
	project(Plan, Init, _Finish).
	% El plan puede ejecutarse con éxito a partir del estado actual. Si alguna de las precondiciones de las
        % acciones del plan ya no se satisfacen (por ejemplo, el tesoro que voy a juntar ya no se encuentra más
        % en la posición que recordaba), entonces project/3 fallará, reflejando que el plan no es factible.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        AGENT SETUP       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      & REGISTRATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic name/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag
%
% Solicita la registración al juego, y recuerda su nombre.


start_ag:- AgName = catulo,
           agent_init(AgName),
           assert(ag_name(AgName)),
	   agent_reset,
           connect,
           run,
           disconnect.

s:- start_ag.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag_instance(+InstanceID)
%
% Solicita la registración al juego de una instancia, y recuerda su
% nombre, que será el de la versión original seguido del InstanceID
% entre paréntesis.


start_ag_instance(InstanceID):-
                    AgClassName = catulo,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).













































































