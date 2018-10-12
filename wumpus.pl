
/* --  Course      : COMP90048 Declarative Programming                      */
/* --  Purpose     : Project 4                                              */
/* --  File        : wumpus.pl                                              */
/* --  Size        : 26.0 KB                                                */
/* --  Stu_id      : xueyangd1                                              */
/* --  Author      : Xueyang Ding 						              	    */
/* --  Modified    : 2018-05-20 14:36:03                                    */

/* Introduction to the project: For the wumpus game, this program aims to   */
/* find and kill the wumpus hiden in the map. The logic of this program is  */
/* firstly trying to discover the map as much as possible, then it will     */
/* judge if wumpus can be killed directly. If not, it will find a shooting  */
/* point to kill it remotely. If no wumpus appeared, it will pick unvisited */
/* points, and shoot them one by one.                                       */

/* ------------------------------------------------------------------------ */
:- module(wumpus,[initialState/5, guess/3, updateState/4]).
/* In initialState the map will be generated, with all the points given     */
/* value shadow except startpoint set empty, the map example is listed below*/
/* e.g. a 3*3 maze, start at point(1,1).The matrix will be:(with wall added)*/
/*  [[wall, wall, wall, wall, wall],                                        */
/*  [wall,empty,shadow,shadow,wall],                                        */
/*  [wall,shadow,shadow,shadow,wall],                                       */
/*  [wall,shadow,shadow,shadow,wall],                                       */
/*  [wall, wall, wall, wall, wall]]                                         */
/* At beginning a circle of walls will be added to the map (For convenience,*/
/* Some later functions don't need to judge out of map error)               */
/* The State stores previous map, new map, a copy of new map,startpoint,    */
/* the size of map, and which process the guess has arrived                 */

initialState(NR, NC, XS, YS, State0) :-  
	matrix(NR,NC,Map),setValue(XS,YS,empty,Map,MedMap),
	addWall_2(MedMap,NewMap),MapCopy = NewMap,LastMap = NewMap,
	X is XS+1,Y is YS+1,R is NR+2,C is NC+2,GuessProcess=start,
	append([LastMap],[NewMap,MapCopy,X,Y,R,C,GuessProcess],State0).
/* ------------------------------------------------------------------------ */
/* functions to build initial map, give value 'shadow' to every point */
matrix(R,C,Matrix) :-
   length(Matrix, R),
   maplist(build(C),Matrix).

build(C, List)  :- 
    length(List, C), 
    maplist(=(shadow), List).
/* -------------------------------------------------------------------------*/
/* functions to get & set a value in map, given the coordinate */
getValue(XS,YS,M,Value) :-
	X is XS-1,Y is YS-1,nth0(Y,M,L),nth0(X,L,Value).

setValue(XS,YS,Value,M,NewM) :-
	X is XS-1,Y is YS-1,
	append(Front,[L|End],M),
	append(Front,[NL|End],NewM),
	length(Front,Y),
	append(ListF,[_|ListE],L),
	append(ListF,[Value|ListE],NL),
	length(ListF,X).
/* -------------------------------------------------------------------------*/
/* functions to add a circle of walls */
/* addWall_1 bulids the left and right boundaries */
/* addWall_2 bulids the top and bottom boundaries*/
addWall_1([],[]).
addWall_1([F|E],[NF|NE]) :-
	append([wall],F,Med),
	append(Med,[wall],NF),
	addWall_1(E,NE).

addWall_2(M,NM) :-
	addWall_1(M,MedM),buildList(wall,Len,L),
	last(MedM,Last),length(Last,Len),
	append([L],MedM,Med2M),
	append(Med2M,[L],NM).

buildList(X, N, List)  :- 
    length(List, N), 
    maplist(=(X), List).
/* -------------------------------------------------------------------------*/
/* functions to check a if two maps are identical */
ifSameMap([],[]).
ifSameMap([MF1|ME1],[MF2|ME2]) :-
	ifSameList(MF1,MF2),ifSameMap(ME1,ME2).

ifSameList([],[]).
ifSameList([F1|E1],[F2|E2]) :-
	F1 = F2,ifSameList(E1,E2).
/* -------------------------------------------------------------------------*/
/* The guess function can be divided into five processes:                   */
/* Firstly, robots try to discover each point of map, until the New map is  */
/* identical with last map. 	                                            */
/* Secondly, the robots use same discovering login in first approach and it */
/* will shoot every time when it faces the pit.                             */
/* Thirdly, robots check if wumpus shows up, if it can kill it with face    */
/* shooting (means dont need to kill with remote shoot)                     */
/* Fourthly, robots check if wumpus shows up, and find an appropriate remote*/
/* point to kill it.                                                        */
/* Lastly, robots will shoot unvisited points one by one, using fourth logic*/
/* start: means the first guess, and last map is same as new map. this runs */
/* only one time, and the status will change to laterGuess                  */
/* laterGuess: do repeatly until last map is identical with new map and it  */
/* will jump to shooting pits using same discovering logic, this runs only  */
/* one time                                                                 */
/* timeToShoot: check if it can kill wumpus directly, if it can, generate   */
/* a guess to kill it, if it can't, jump to next logic: remote shoot. If no */
/* wumpus, it will jump to shoot the unvisited points one by one            */
/* For each robot, 100 energy will be used                                  */
guess(State0, State, Guess) :- 
/* Firstly, robots try to discover each point of map, this only run one time*/
/* because firsly the last map is identical with new map(in order to avoid) */
last(State0,start),nth0(0,State0,LastMap),nth0(1,State0,NewMap),
nth0(2,State0,MapCopy),nth0(3,State0,XS),nth0(4,State0,YS),
nth0(5,State0,NR),nth0(6,State0,NC),
makeGuess(NR,NC,XS,YS,MapCopy,shoot,Guess,100),
append([LastMap],[NewMap,MapCopy,XS,YS,NR,NC,laterGuess],State);

/* laterGuess: do repeatly until last map is identical with new map and it  */
last(State0,laterGuess),nth0(0,State0,LastMap),nth0(2,State0,MapCopy),
nth0(3,State0,XS),nth0(4,State0,YS),nth0(5,State0,NR),nth0(6,State0,NC),
not(ifSameMap(LastMap,MapCopy)),
makeGuess(NR,NC,XS,YS,MapCopy,shoot,Guess,100),State = State0;

/* Secondly, the robots use same discovering login in first approach and it */
/* will shoot every time when it faces the pit.                             */
last(State0,laterGuess),nth0(0,State0,LastMap),nth0(1,State0,NewMap),
nth0(2,State0,MapCopy),nth0(3,State0,XS),nth0(4,State0,YS),nth0(5,State0,NR),
nth0(6,State0,NC),goWithShoot(NR,NC,XS,YS,MapCopy,shoot,Guess,100),
append([LastMap],[NewMap,MapCopy,XS,YS,NR,NC,timeToShoot],State);

/* Thirdly, robots check if wumpus shows up, if it can kill it with face    */
/* shooting (means dont need to kill with remote shoot)                     */
last(State0,timeToShoot),nth0(2,State0,MapCopy),
nth0(3,State0,XS),nth0(4,State0,YS),
directlykillWumpus(XS,YS,MapCopy,Guess);

/* Fourthly, robots check if wumpus shows up, and find an appropriate remote*/
/* point to kill it.                                                        */                                    
last(State0,timeToShoot),nth0(2,State0,MapCopy),nth0(3,State0,XS),
nth0(4,State0,YS),nth0(5,State0,NR),nth0(6,State0,NC),
guessToKillWumpus(NR,NC,XS,YS,MapCopy,Guess);

/* Lastly, robots will shoot unvisited points one by one, using fourth logic*/
last(State0,timeToShoot),nth0(1,State0,M),nth0(2,State0,MapCopy),
nth0(3,State0,XS),nth0(4,State0,YS),nth0(5,State0,NR),nth0(6,State0,NC),
pickUnvisitedPlace(MapCopy,1,ShadowX,ShadowY),
shootUnvisitPlace(NR,NC,XS,YS,ShadowX,ShadowY,MapCopy,Guess),
setValue(ShadowX,ShadowY,shoted,MapCopy,NewM),
append([NewM],[NewM,MapCopy,XS,YS,NR,NC,timeToShoot],State).

/* -------------------------------------------------------------------------*/
/* The map will be updated everytime, using last guess and feedback */
updateState(State0, Guess, Feedback, State) :- 
	nth0(1,State0,M),nth0(3,State0,XS),nth0(4,State0,YS),
	nth0(5,State0,NR),nth0(6,State0,NC),nth0(7,State0,GuessProcess),
	updateMatrix(XS,YS,M,NR,NC,Guess,Feedback,NM),MapCopy = NM,
	append([M],[NM,MapCopy,XS,YS,NR,NC,GuessProcess],State).
/* -------------------------------------------------------------------------*/
/* This funcion is for updating matrix by giving a list of guess and        */
/* feedback. It is called recursively. if the feedback is smell,damp or miss*/
/* it do nothing, otherwise it will write the feedback into map.            */
updateMatrix(XS,YS,M,NR,NC,[],_,M).
updateMatrix(XS,YS,M,NR,NC,_,[],M).
updateMatrix(XS,YS,M,NR,NC,[Gs|GTail],[Fd|FTail],NM1) :-
	((Fd = smell;Fd = damp),
		updateMatrix(XS,YS,M,NR,NC,[Gs|GTail],[empty|FTail],NM1));
	(Fd = miss,updateMatrix(XS,YS,M,NR,NC,GTail,FTail,NM1));
		((Gs = north, X is XS, Y is YS - 1);
		(Gs = east, X is XS + 1, Y is YS);
		(Gs = south, X is XS, Y is YS + 1);
		(Gs = west, X is XS - 1, Y is YS)),
	(((Fd = empty;Fd = pit;Fd=wumpus;Fd = stench),setValue(X,Y,Fd,M,NM),
		updateMatrix(X,Y,NM,NR,NC,GTail,FTail,NM1));
		(Fd = wall,setValue(X,Y,Fd,M,NM),
		updateMatrix(XS,YS,NM,NR,NC,GTail,FTail,NM1))).

/*--------------------------------------------------------------------------*/
/*These are the functions for the first step: discovering the map           */
/*makeGuess is for generateing guess for a robot, the logic of this function*/
/*is: robot will check its surroundings, if north point is empty or shadow  */
/*or stench, it will go north firstly, otherwise it will check its east     */
/*point,same logic. Then it will check south point, and then west point.    */
/*If it goes into a dead end, it will go back one step, using accumulator as*/
/*a stack to store the guesses that have made. If the robot go back to      */
/*startpoint, the function will stop.                                       */
makeGuess(NR,NC,XS,YS,MapCopy,G,[Gs|GTail],Energy) :-
SX is XS,SY is YS,makeGuess(NR,NC,XS,YS,MapCopy,G,[Gs|GTail],Energy,A,SX,SY).

makeGuess(_,_,_,_,_,_,[],Energy,_,_,_) :- Energy =:= 0.
makeGuess(_,_,XS,YS,MapCopy,_,[],Energy,[],SX,SY) :- 
	ifInDeadEnd(XS,YS,MapCopy),XS=SX,YS=SY.
makeGuess(NR,NC,XS,YS,MapCopy,G,[Gs|GTail],Energy,Acc,SX,SY) :-

	(XN is XS,YN is YS-1,getValue(XN,YN,MapCopy,V),
	(V = empty;V = shadow;V = stench),Gs = north,NewE is Energy-1,
	setValue(XS,YS,visited,MapCopy,NMC),append(Acc,[Gs],NewAcc),
	makeGuess(NR,NC,XN,YN,NMC,north,GTail,NewE,NewAcc,SX,SY));

	(XE is XS+1,YE is YS,getValue(XE,YE,MapCopy,V),
	(V = empty;V = shadow;V = stench),Gs = east,NewE is Energy-1,
	setValue(XS,YS,visited,MapCopy,NMC),append(Acc,[Gs],NewAcc),
	makeGuess(NR,NC,XE,YE,NMC,east,GTail,NewE,NewAcc,SX,SY));

	(XSS is XS,YSS is YS+1,getValue(XSS,YSS,MapCopy,V),
	(V = empty;V = shadow;V = stench),Gs = south,NewE is Energy -1,
	setValue(XS,YS,visited,MapCopy,NMC),append(Acc,[Gs],NewAcc),
	makeGuess(NR,NC,XSS,YSS,NMC,south,GTail,NewE,NewAcc,SX,SY));

	(XW is XS-1,YW is YS,getValue(XW,YW,MapCopy,V),
	(V = empty;V = shadow;V = stench),Gs = west,NewE is Energy -1,
	setValue(XS,YS,visited,MapCopy,NMC),append(Acc,[Gs],NewAcc),
	makeGuess(NR,NC,XW,YW,NMC,west,GTail,NewE,NewAcc,SX,SY));

	(ifInDeadEnd(XS,YS,MapCopy),last(Acc,OpGs),turnBack(OpGs,Gs),
	getNewPosition(XS,YS,Gs,XNew,YNew),deleteLast(Acc,NewAcc),
	setValue(XS,YS,wall,MapCopy,NMC),NewE is Energy-1,
	makeGuess(NR,NC,XNew,YNew,NMC,Gs,GTail,NewE,NewAcc,SX,SY)).

/* check if robot enters a dead end */
ifInDeadEnd(XS,YS,MapCopy) :-
	X1 is XS+1,X2 is XS-1,Y1 is YS+1,Y2 is YS-1,
	getValue(X1,YS,MapCopy,V1),getValue(X2,YS,MapCopy,V2),
	getValue(XS,Y1,MapCopy,V3),getValue(XS,Y2,MapCopy,V4),
	(V1=wall;V1=pit;V1=wumpus;V1=visited),
	(V2=wall;V2=pit;V2=wumpus;V2=visited),
	(V3=wall;V3=pit;V3=wumpus;V3=visited),
	(V4=wall;V4=pit;V4=wumpus;V4=visited).

/* input the previous guess, get next opposite guess to go out of dead end */
turnBack(Gs1,Gs2) :-
	Gs1 = north,Gs2 = south;
	Gs1 = south,Gs2 = north;
	Gs1 = west,Gs2 = east;
	Gs1 = east,Gs2 = west.

/* used to delete the last guess in accumulator */
deleteLast([X|Xs], Ys) :-                 
   prev(Xs, Ys, X).            

prev([], [], _).
prev([X1|Xs], [X0|Ys], X0) :-  
   prev(Xs, Ys, X1).

/* used to get previous position when robot returns back one step*/
getNewPosition(XS,YS,Gs,XNew,YNew) :-
	Gs = north,XNew is XS,YNew is YS - 1;
	Gs = east,XNew is XS+1,YNew is YS;
	Gs = south,XNew is XS,YNew is YS+1;
	Gs = west,XNew is XS-1,YNew is YS.

/*--------------------------------------------------------------------------*/
/* These are the functions for the second step: shoot pits if it faces      */
/* goWithShoot uses similar logic with makeGuess function, the only thing   */
/* changed is that it adds ifShoot function before checking surroundings    */
goWithShoot(NR,NC,XS,YS,MapCopy,G,[Gs|GTail],Energy) :-
	SX is XS,SY is YS,
	goWithShoot(NR,NC,XS,YS,MapCopy,G,[Gs|GTail],Energy,A,SX,SY).

goWithShoot(_,_,_,_,_,_,[],Energy,_,_,_) :- Energy =:= 0.
goWithShoot(_,_,XS,YS,MapCopy,_,[],Energy,[],SX,SY) :- 
	ifInDeadEnd(XS,YS,MapCopy),XS=SX,YS=SY.
goWithShoot(NR,NC,XS,YS,MapCopy,G,[Gs|GTail],Energy,Acc,SX,SY) :-
	(ifShoot(NR,NC,XS,YS,MapCopy,G,[Gs|GTail],Energy,Acc,SX,SY));

	(XN is XS,YN is YS-1,getValue(XN,YN,MapCopy,V),
		(V = empty;V = shadow;V = stench),NewE is Energy-1,Gs = north,
		setValue(XS,YS,visited,MapCopy,NMC),append(Acc,[Gs],NewAcc),
		goWithShoot(NR,NC,XN,YN,NMC,north,GTail,NewE,NewAcc,SX,SY));

	(XE is XS+1,YE is YS,getValue(XE,YE,MapCopy,V),
		(V = empty;V = shadow;V = stench),NewE is Energy-1,Gs = east,
		setValue(XS,YS,visited,MapCopy,NMC),append(Acc,[Gs],NewAcc),
		goWithShoot(NR,NC,XE,YE,NMC,east,GTail,NewE,NewAcc,SX,SY));

	(XSS is XS,YSS is YS+1,getValue(XSS,YSS,MapCopy,V),
		(V = empty;V = shadow;V = stench),NewE is Energy-1,Gs = south,
		setValue(XS,YS,visited,MapCopy,NMC),append(Acc,[Gs],NewAcc),
		goWithShoot(NR,NC,XSS,YSS,NMC,south,GTail,NewE,NewAcc,SX,SY));

	(XW is XS-1,YW is YS,getValue(XW,YW,MapCopy,V),
		(V = empty;V = shadow;V = stench),NewE is Energy -1,Gs = west,
		setValue(XS,YS,visited,MapCopy,NMC),append(Acc,[Gs],NewAcc),
		goWithShoot(NR,NC,XW,YW,NMC,west,GTail,NewE,NewAcc,SX,SY));

	(ifInDeadEnd(XS,YS,MapCopy),last(Acc,OpGs),turnBack(OpGs,Gs),
		getNewPosition(XS,YS,Gs,XNew,YNew),deleteLast(Acc,NewAcc),
		setValue(XS,YS,wall,MapCopy,NMC),NewE is Energy-1,
		goWithShoot(NR,NC,XNew,YNew,NMC,Gs,GTail,NewE,NewAcc,SX,SY)).

/* This function is added as the first judgement sentence in goWithShoot    */
/* for each point the robot moved in,it robot will check its face, if robot */
/* faces a pit or a wumpus, it will shoot                                   */
ifShoot(NR,NC,XS,YS,MapCopy,G,[Gs|GTail],Energy,Acc,SX,SY) :-
	(Energy>=5,XN is XS,YN is YS-1,getValue(XN,YN,MapCopy,V),
		(V = pit;V = wumpus),G = north,Gs = shoot,NewE is Energy-5,
		goWithShoot(NR,NC,XS,YS,MapCopy,shoot,GTail,NewE,Acc,SX,SY));
	(Energy>=5,XE is XS+1,YE is YS,getValue(XE,YE,MapCopy,V),
		(V = pit;V = wumpus),G = east,Gs = shoot,NewE is Energy-5,
		goWithShoot(NR,NC,XS,YS,MapCopy,shoot,GTail,NewE,Acc,SX,SY));
	(Energy>=5,XSS is XS,YSS is YS+1,getValue(XSS,YSS,MapCopy,V),
		(V = pit;V = wumpus),G = south,Gs = shoot,NewE is Energy-5,
		goWithShoot(NR,NC,XS,YS,MapCopy,shoot,GTail,NewE,Acc,SX,SY));
	(Energy>=5,XW is XS-1,YW is YS,getValue(XW,YW,MapCopy,V),
		(V = pit;V = wumpus),G = west,Gs = shoot,NewE is Energy-5,
		goWithShoot(NR,NC,XW,YW,MapCopy,shoot,GTail,NewE,Acc,SX,SY)).
/*--------------------------------------------------------------------------*/
/* These are the functions for the third step:kill wumpus with face shoot   */
/* ifWumpus will return the wumpus's coordinate if the wumpus has found,    */
/* otherwise it returns false                                               */
ifWumpus([F|T],Count,X,Y) :-                  
	nth0(X1,F,wumpus),X is X1+1,Y is Count;
	NewC is Count+1,ifWumpus(T,NewC,X,Y).

/* ifFaceShoot will check wumpus's surroundings: only two adjacent empty    */
/* spaces connected to wumpus's directly, the function will return true     */
ifFaceShoot(X,Y,M) :-
	XN1 is X,XN2 is X,YN1 is Y-1,YN2 is Y-2,getValue(XN1,YN1,M,VN1),
	getValue(XN2,YN2,M,VN2),VN1=stench,VN2=empty;
	XS1 is X,XS2 is X,YS1 is Y+1,YS2 is Y+2,getValue(XS1,YS1,M,VS1),
	getValue(XS2,YS2,M,VS2),VS1=stench,VS2=empty;
	XW1 is X-1,XW2 is X-2,YW1 is Y,YW2 is Y,getValue(XW1,YW1,M,VW1),
	getValue(XW2,YW2,M,VW2),VW1=stench,VW2=empty;
	XE1 is X+1,XE2 is X+2,YE1 is Y,YE2 is Y,getValue(XE1,YE1,M,VE1),
	getValue(XE2,YE2,M,VE2),VE1=stench,VE2=empty.

/* directlykillWumpus will return a list of guess if Wumpus found and if    */
/* can face shoot. the last guess will change from meeting the wumpus to    */
/* shoot. It forces that the last two steps to wumpus are same step. So     */
/* the last step changing to shoot can kill wumpus                          */
directlykillWumpus(XS,YS,MapC,Guess) :-
	ifWumpus(MapC,1,X,Y),ifFaceShoot(X,Y,MapC),find((XS,YS),(X,Y),MapC,Dirn),
	last(Dirn,Last),append(Front,[Last],Dirn),last(Front,Last),
	append(Front,[shoot],Guess).

/* find function will return a list of guess from the startpoint (XS,YS)    */
/* to any points (X,Y), and the route will evade the wall and pit           */
find((XS,YS),(X,Y),Map,Dirn) :-
	find((XS,YS),(X,Y),Map,[(XS,YS)],Path,Dirn).

find((XS,YS),(XS,YS),Map,_Previous,[],[]).
find((XS,YS),(X,Y),Map,Previous,[Next|Path],[Dirn|PreDirn]) :-
    edge1(Map,(XS,YS),(MedX,MedY),Next,Dirn),
    not(member((MedX,MedY), Previous)),                        
    find((MedX,MedY),(X,Y),Map,[(MedX,MedY)|Previous],Path,PreDirn).
 
/* Only the edges with empty,stench or wumpus are legal edge                */
edge1(Map,(X1,Y1),(X2,Y2),Next,Dirn) :-
	(X2 is X1+1,Y2 is Y1,getValue(X2,Y2,Map,V),
		(V = empty;V = stench;V = wumpus),Next = (X2,Y2),Dirn = east);
	(X2 is X1-1,Y2 is Y1,getValue(X2,Y2,Map,V),
		(V = empty;V = stench;V = wumpus),Next = (X2,Y2),Dirn = west);
	(X2 is X1,Y2 is Y1+1,getValue(X2,Y2,Map,V),
		(V = empty;V = stench;V = wumpus),Next = (X2,Y2),Dirn = south);
	(X2 is X1,Y2 is Y1-1,getValue(X2,Y2,Map,V),
		(V = empty;V = stench;V = wumpus),Next = (X2,Y2),Dirn = north).
/*--------------------------------------------------------------------------*/
/* These are the functions for the fourth step:kill wumpus remotely         */
/* guessToKillWumpus function will return a list of guess to kill wumpus    */
/* It firstly checks if wumpus found, and then it will find shoot place     */
/* It uses findR function to go to the shoot place, and uses latestep to    */
/* turn head:facing the direction that wumpus at,then shoot                 */
guessToKillWumpus(NR,NC,XS,YS,MapC,Guess) :-
	ifWumpus(MapC,1,X,Y),findShootPlace(NR,NC,X,Y,MapC,PlaceX,PlaceY),
	findR((XS,YS),(PlaceX,PlaceY),MapC,Dirn),
	noWallBetween((PlaceX,PlaceY),(X,Y),MapC),
	lastStep(PlaceX,PlaceY,X,Y,Action),
	append(Dirn,[Action,shoot],Guess).

/* for turning head so robot faces the direction that wumpus at.            */
lastStep(PlaceX,PlaceY,WX,WY,Action) :-
	PlaceX = WX,PlaceY < WY,Action = south;
	PlaceX = WX,PlaceY > WY,Action = north;
	PlaceY = WY,PlaceX < WX,Action = east;
	PlaceY = WY,PlaceX > WX,Action = west.

/* findShootPlace function will return a coordinate that satisfies:         */
/* robot stands at this point can move a step to face to wumpus             */
/* it will return one position by checking wumpus's north,south,west,east   */
findShootPlace(NR,NC,X0,Y0,M,PlaceX,PlaceY) :-
	findNorthShoot(NR,NC,X0,Y0,M,PlaceX,PlaceY);
	findSouthShoot(NR,NC,X0,Y0,M,PlaceX,PlaceY);
	findWestShoot(NR,NC,X0,Y0,M,PlaceX,PlaceY);
	findEastShoot(NR,NC,X0,Y0,M,PlaceX,PlaceY).

findNorthShoot(NR,NC,X0,Y0,M,PlaceX,PlaceY) :-
	PlaceX is X0,YPre is Y0-1,PlaceY is Y0-2,getValue(PlaceX,YPre,M,VPre),
	getValue(PlaceX,PlaceY,M,V),VPre = empty,V = empty;
	YPre is Y0-1,YPre > 2,findNorthShoot(NR,NC,X0,YPre,M,PlaceX,PlaceY).

findSouthShoot(NR,NC,X0,Y0,M,PlaceX,PlaceY) :-
	PlaceX is X0,YPre is Y0+1,PlaceY is Y0+2,getValue(PlaceX,YPre,M,VPre),
	getValue(PlaceX,PlaceY,M,V),VPre = empty,V = empty;
	YPre is Y0+1,N is NR-1,YPre < N,
	findSouthShoot(NR,NC,X0,YPre,M,PlaceX,PlaceY).

findWestShoot(NR,NC,X0,Y0,M,PlaceX,PlaceY) :-
	PlaceY is Y0,XPre is X0-1,PlaceX is X0-2,getValue(XPre,PlaceY,M,VPre),
	getValue(PlaceX,PlaceY,M,V),VPre = empty,V = empty;
	XPre is X0-1,XPre > 2,
	findWestShoot(NR,NC,XPre,Y0,M,PlaceX,PlaceY).

findEastShoot(NR,NC,X0,Y0,M,PlaceX,PlaceY) :-
	PlaceY is Y0,XPre is X0+1,PlaceX is X0+2,getValue(XPre,PlaceY,M,VPre),
	getValue(PlaceX,PlaceY,M,V),VPre = empty,V = empty;
	XPre is X0+1,N is NC-1,XPre < N,
	findEastShoot(NR,NC,XPre,Y0,M,PlaceX,PlaceY).


/* this function is for ensuring that there is no wall between shooting     */
/* point and wumpus.                                                        */
noWallBetween((X,Y),(X,Y),Map).
noWallBetween((PlaceX,PlaceY),(X,Y),Map) :-
	PlaceX = X,PlaceY = Y;
	PlaceX=X,MidY is PlaceY-1,MidY >= Y,getValue(X,MidY,Map,V),
	V \= wall,noWallBetween((PlaceX,MidY),(X,Y),Map);
	PlaceX=X,MidY is PlaceY+1,MidY =< Y,getValue(X,MidY,Map,V),
	V \= wall,noWallBetween((PlaceX,MidY),(X,Y),Map);
	PlaceY=Y,MidX is PlaceX-1,MidX >= X,getValue(MidX,Y,Map,V),
	V \= wall,noWallBetween((MidX,PlaceY),(X,Y),Map);
	PlaceY=Y,MidX is PlaceX+1,MidX =< X,getValue(MidX,Y,Map,V),
	V \= wall,noWallBetween((MidX,PlaceY),(X,Y),Map).



/* -------------------------------------------------------------------------*/
/* findR function will return a list of guess from the startpoint (XS,YS)   */
/* to any points (X,Y), and the route will evade the wall,pit and wumpus    */
/* notice: it is a bit different with find: it is used for finding shoot    */
/* place, in its edge2 function, the V = wumpus is deleted                  */
findR((XS,YS),(X,Y),Map,Dirn) :-
	findR((XS,YS),(X,Y),Map,[(XS,YS)],Path,Dirn).

findR((XS,YS),(XS,YS),Map,_Previous,[],[]).
findR((XS,YS),(X,Y),Map,Previous,[Next|Path],[Dirn|PreDirn]) :-
    edge2(Map,(XS,YS),(MedX,MedY),Next,Dirn),
    not(member((MedX,MedY), Previous)),                        
    findR((MedX,MedY),(X,Y),Map,[(MedX,MedY)|Previous],Path,PreDirn).

edge2(Map,(X1,Y1),(X2,Y2),Next,Dirn) :-
	(X2 is X1+1,Y2 is Y1,getValue(X2,Y2,Map,V),
		(V = empty;V = stench),Next = (X2,Y2),Dirn = east);
	(X2 is X1-1,Y2 is Y1,getValue(X2,Y2,Map,V),
		(V = empty;V = stench),Next = (X2,Y2),Dirn = west);
	(X2 is X1,Y2 is Y1+1,getValue(X2,Y2,Map,V),
		(V = empty;V = stench),Next = (X2,Y2),Dirn = south);
	(X2 is X1,Y2 is Y1-1,getValue(X2,Y2,Map,V),
		(V = empty;V = stench),Next = (X2,Y2),Dirn = north).
/* -------------------------------------------------------------------------*/
/* These are the functions for the last step:shoot unvisited points         */
/* pickUnvisitedPlace function is for picking any shadow point and the      */
/* shootUnvisitPlace is for shooting this point, pretty much same with      */
/* remote shooting, it uses many functions in fourth step                   */
pickUnvisitedPlace([F|T],Count,X,Y) :-   
	nth0(X1,F,shadow),X is X1+1,Y is Count;
	NewC is Count+1,pickUnvisitedPlace(T,NewC,X,Y).

shootUnvisitPlace(NR,NC,XS,YS,ShadowX,ShadowY,MapC,Guess) :-
	findShootPlace(NR,NC,ShadowX,ShadowY,MapC,PlaceX,PlaceY),
	noWallBetween((PlaceX,PlaceY),(ShadowX,ShadowY),MapC),
	find((XS,YS),(PlaceX,PlaceY),MapC,Dirn),
	lastStep(PlaceX,PlaceY,ShadowX,ShadowY,Action),
	append(Dirn,[Action,shoot],Guess).
/* -------------------------------------------------------------------------*/