%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% MODULES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(random)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% INITIALIZATION %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%			  
start:-
		akari([
		[_,_,_,10,_,_,_],
		[_,13,_,_,_,15,_],
		[_,_,15,_,15,_,_],
		[15,_,_,15,_,_,13],
		[_,_,12,_,15,_,_],
		[_,12,_,_,_,15,_],
		[_,_,_,11,_,_,_]],7).
		
start2:-
		akari([
		[_,_,_,10,12,_,_,_,_,_],
		[_,15,_,_,_,_,11,_,11,_],
		[_,_,15,_,_,_,_,12,_,_],
		[_,12,_,_,_,10,_,_,_,15],
		[_,_,_,15,_,_,_,_,_,10],
		[15,_,_,_,_,_,15,_,_,_],
		[10,_,_,_,10,_,_,_,15,_],
		[_,_,12,_,_,_,_,10,_,_],
		[_,15,_,13,_,_,_,_,11,_],
		[_,_,_,_,_,15,11,_,_,_]],10).
		
start3:-
		akari([
			[_,_,_,_,_,11,_,_,_,_,_,15,_,_,_,_,_,_,_,_,11,_,_,11,_],
			[15,_,_,15,_,15,_,11,_,15,_,12,_,_,12,_,15,_,12,_,_,_,_,_,_],
			[_,_,11,_,_,_,11,15,_,_,_,_,_,_,_,_,_,15,15,_,_,15,12,_,_],
			[_,_,12,_,_,_,_,_,_,10,15,_,_,10,_,11,_,_,_,15,_,_,_,15,_],
			[15,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,15,_,_,_,_,_,_,_,_],
			[_,_,_,15,_,11,_,15,_,_,15,_,_,_,_,10,15,_,13,15,_,_,_,15,15],
			[_,11,15,_,_,15,_,_,_,15,11,_,15,_,_,_,_,_,_,_,_,_,15,_,_],
			[_,_,11,_,_,_,_,12,15,_,15,_,_,15,_,13,_,15,_,15,_,_,10,15,_],
			[_,11,_,_,15,15,_,_,_,12,_,15,11,_,15,_,_,15,_,_,_,_,_,_,_],
			[_,_,_,10,_,15,_,13,_,_,_,_,15,_,_,_,15,_,15,_,_,15,_,11,_],
			[_,11,_,_,_,_,_,_,15,_,_,_,15,12,_,_,_,15,15,10,_,10,_,_,_],
			[_,_,_,11,_,_,_,11,_,_,15,_,15,_,_,_,11,_,_,_,_,_,_,10,15],
			[_,_,_,_,_,_,15,_,15,15,15,12,15,15,15,15,11,_,12,_,_,_,_,_,_],
			[15,15,_,_,_,_,_,_,15,_,_,_,15,_,15,_,_,12,_,_,_,15,_,_,_],
			[_,_,_,10,_,15,15,13,_,_,_,15,15,_,_,_,11,_,_,_,_,_,_,15,_],
			[_,15,_,15,_,_,15,_,12,_,_,_,15,_,_,_,_,15,_,10,_,15,_,_,_],
			[_,_,_,_,_,_,_,15,_,_,15,_,15,15,_,15,_,_,_,11,11,_,_,15,_],
			[_,13,11,_,_,15,_,15,_,15,_,13,_,_,11,_,15,15,_,_,_,_,15,_,_],
			[_,_,15,_,_,_,_,_,_,_,_,_,13,_,15,13,_,_,_,15,_,_,15,12,_],
			[15,11,_,_,_,10,15,_,15,10,_,_,_,_,12,_,_,10,_,15,_,15,_,_,_],
			[_,_,_,_,_,_,_,_,15,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,11],
			[_,15,_,_,_,13,_,_,_,15,_,10,_,_,15,15,_,_,_,_,_,_,11,_,_],
			[_,_,11,10,_,_,15,12,_,_,_,_,_,_,_,_,_,15,11,_,_,_,10,_,_],
			[_,_,_,_,_,_,15,_,15,_,15,_,_,15,_,15,_,12,_,15,_,11,_,_,15],
			[_,15,_,_,10,_,_,_,_,_,_,_,_,11,_,_,_,_,_,15,_,_,_,_,_]],25).
			
start4:- akari([
				[15,15,15],
				[15,15,15],
				[15,15,15]],3).

start5:-
		akari([
		[_,_,_,15,_,_,_],
		[_,13,_,_,_,13,_],
		[_,_,_,_,_,_,_],
		[10,_,_,_,_,_,15],
		[_,_,_,_,_,_,_],
		[_,10,_,_,_,13,_],
		[_,_,_,15,_,_,_]],7).

start6:-
		akari([
		[_,_,15,_,_,_,_,13,_,_],
		[_,_,_,_,_,_,15,_,_,_],
		[12,_,15,_,_,10,_,12,_,15],
		[_,12,_,_,_,_,_,_,_,_],
		[_,_,11,_,_,_,_,_,_,_],
		[_,_,_,_,_,_,_,11,_,_],
		[_,_,_,_,_,_,_,_,13,_],
		[10,_,12,_,12,_,_,15,_,13],
		[_,_,_,15,_,_,_,_,_,_],
		[_,_,11,_,_,_,_,10,_,_]],10).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%% MAIN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
akari(Rows, Size) :-
	length(Rows, Size), maplist(length_list(Size), Rows),
	append(Rows, Board),
	domain(Board, 0, 15),
	processRows(Rows, Rows, 0-0, Size),
	reset_timer,
	labeling([], Board),
	print_time,
	fd_statistics,
	printBoard(Board, Size).			
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%% PROCESSING ROWS -> PLURAL %%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
processRows([], _Board, _Row-_Col, _Size).

processRows([H|T], Board, Row-Col, Size):-
	processRow(H, Board, Row-Col, Size, []),
	NewRow is Row + 1,
	processRows(T, Board, NewRow-0, Size).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% PROCESSING ROW -> SINGULAR %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
processRow([], _Board, _Row-_Col, _Size, _VarList).

processRow([H|T], Board, Row-Col, Size, _VarList):-
	H == 15,
	NewCol is Col + 1,
	processRow(T, Board, Row-NewCol, Size, []).
					
processRow([H|T], Board, Row-Col, Size, _VarList):-
	compare(H),
	hintRestriction(Board, Row-Col, H, Size),
	NewCol is Col + 1,
	processRow(T, Board, Row-NewCol, Size, []).
											
processRow([H|T], Board, Row-Col, Size, VarList):-
	insertAtEnd(H, [], L),
	sum(L, #=<, 1), % each space can have at most one light
	gatherLeft(Board, Row-Col, Row-Col, Size, VarList),
	NewCol is Col + 1,
	processRow(T, Board, Row-NewCol, Size, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% RESTRICT HINTS %%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hintRestriction(Board, Row-Col, NumberHigh, Size):-
	Number is NumberHigh - 10,			
	NewRow1 is Row - 1,
	getUpSquare(Board, NewRow1-Col, Size, US),
	append(US, [], List),
	NewRow2 is Row + 1,
	getDownSquare(Board, NewRow2-Col, Size, DS),
	append(DS, List, List2),
	NewCol1 is Col - 1,
	getLeftSquare(Board, Row-NewCol1, Size, LS),
	append(LS, List2, List3),
	NewCol2 is Col + 1,
	getRightSquare(Board, Row-NewCol2, Size, RS),
	append(RS, List3, FinalList),
	sum(FinalList, #=, Number).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% GET UP SQUARE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%								
getUpSquare(_Board, Row-_Col, _Size, US):-
	Row < 0, 
	US = [].
										
getUpSquare(Board, Row-Col, _Size, US):-
	getElem(Board, Row-Col, Elem),
	compare(Elem),
	US = [].		
										
getUpSquare(Board, Row-Col, _Size, US):-
	getElem(Board, Row-Col, Elem),
	US = [Elem].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% GET DOWN SQUARE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%										
getDownSquare(_Board, Row-_Col, Size, DS):-
	Row >= Size,
	DS = [].

getDownSquare(Board, Row-Col, _Size, DS):-
	getElem(Board, Row-Col, Elem),
	compare(Elem),
	DS = [].
										
getDownSquare(Board, Row-Col, _Size, DS):-
	getElem(Board, Row-Col, Elem),
	DS = [Elem].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% GET LEFT SQUARE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%										
getLeftSquare(_Board, _Row-Col, _Size, LS):-
	Col < 0,
	LS = [].

getLeftSquare(Board, Row-Col, _Size, LS):-
	getElem(Board, Row-Col, Elem),
	compare(Elem),
	LS = [].
										
getLeftSquare(Board, Row-Col, _Size, LS):-
	getElem(Board, Row-Col, Elem),
	LS = [Elem].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% GET RIGHT SQUARE %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getRightSquare(_Board, _Row-Col, Size, RS):-
	Col >= Size,
	RS = [].

getRightSquare(Board, Row-Col, _Size, RS):-
	getElem(Board, Row-Col, Elem),
	compare(Elem),
	RS = [].
										
getRightSquare(Board, Row-Col, _Size, RS):-
	getElem(Board, Row-Col, Elem),
	RS = [Elem].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% GATHER LEFT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%			
gatherLeft(Board, ORow-OCol, _CRow-CCol, Size, RowList):-
	CCol < 0,
	NewCol is OCol + 1,
	gatherRight(Board, ORow-OCol, ORow-NewCol, Size, RowList).
													
gatherLeft(Board, ORow-OCol, CRow-CCol, Size, RowList):-
	getElem(Board, CRow-CCol, Elem),
	compare(Elem),
	NewCol is OCol + 1,
	gatherRight(Board, ORow-OCol, ORow-NewCol, Size, RowList).
																				
gatherLeft(Board, ORow-OCol, CRow-CCol, Size, RowList):-
	getElem(Board, CRow-CCol, Elem),
	insertAtEnd(Elem, RowList, NewRowList),
	NewCol is CCol - 1,
	gatherLeft(Board, ORow-OCol, CRow-NewCol, Size, NewRowList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% GATHER RIGHT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%				
gatherRight(Board, ORow-OCol, _CRow-CCol, Size, RowList):-
	CCol >= Size,
	sum(RowList, #=<, 1),
	gatherUp(Board, ORow-OCol, ORow-OCol, Size, RowList, []).
													
gatherRight(Board, ORow-OCol, CRow-CCol, Size, RowList):-
	getElem(Board, CRow-CCol, Elem),
	compare(Elem),
	sum(RowList, #=<, 1),
	gatherUp(Board, ORow-OCol, ORow-OCol, Size, RowList, []).
													
gatherRight(Board, ORow-OCol, CRow-CCol, Size, RowList):-
	getElem(Board, CRow-CCol, Elem),
	insertAtEnd(Elem, RowList, NewRowList),
	NewCol is CCol + 1,
	gatherRight(Board, ORow-OCol, CRow-NewCol, Size, NewRowList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% GATHER UP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%										
gatherUp(Board, ORow-OCol, CRow-_CCol, Size, VarList, ColList):-
	CRow < 0,
	NewRow is ORow + 1,
	gatherDown(Board, ORow-OCol, NewRow-OCol, Size, VarList, ColList).
													
gatherUp(Board, ORow-OCol, CRow-CCol, Size, VarList, ColList):-
	getElem(Board, CRow-CCol, Elem),
	compare(Elem),
	NewRow is ORow + 1,
	gatherDown(Board, ORow-OCol, NewRow-OCol, Size, VarList, ColList).
													
gatherUp(Board, ORow-OCol, CRow-CCol, Size, VarList, ColList):-
	getElem(Board, CRow-CCol, Elem),
	insertAtEnd(Elem, VarList, NewVarList),
	insertAtEnd(Elem, ColList, NewColList),
	NewRow is CRow - 1,
	gatherUp(Board, ORow-OCol, NewRow-CCol, Size, NewVarList, NewColList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% GATHER DOWN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%												
gatherDown(_Board, _ORow-_OCol, CRow-_CCol, Size, VarList, ColList):-
	CRow >= Size,
	sum(ColList, #=<, 1),
	sum(VarList, #>=, 1).
													
gatherDown(Board, _ORow-_OCol, CRow-CCol, _Size, VarList, ColList):-
	getElem(Board, CRow-CCol, Elem),
	compare(Elem),
	sum(ColList, #=<, 1),
	sum(VarList, #>=, 1).
																																
gatherDown(Board, ORow-OCol, CRow-CCol, Size, VarList, ColList):-
	getElem(Board, CRow-CCol, Elem),
	insertAtEnd(Elem, VarList, NewVarList),
	insertAtEnd(Elem, ColList, NewColList),
	NewRow is CRow + 1,
	gatherDown(Board, ORow-OCol, NewRow-CCol, Size, NewVarList, NewColList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% GENERATOR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate(Size):-
	length(Rows, Size), maplist(length_list(Size), Rows),
	append(Rows, Board),
	domain(Board, 0, 15),
	boardRestrictions(Rows, Size),
	labeling([], Board),
	printBoard(Board, Size), nl,
	copy(Rows, _, Size, NewFinal),
	akari(NewFinal, Size).

boardRestrictions(Rows, Size):-
	runRows(Rows, Rows, 0-0, Size).
		
runRows([], _Board, _Row-_Col, _Size).

runRows([H|T], Board, Row-Col, Size):-
	runRow(H, Board, Row-Col, Size),
	NewRow is Row + 1,
	runRows(T, Board, NewRow-0, Size).		

runRow([], _Board, _Row-_Col, _Size).
					
runRow([H|T], Board, Row-Col, Size):-
	random(0, 2, Random),
	processSquare(H, Board, Row-Col, Random, Size),
	NewCol is Col + 1,
	runRow(T, Board, Row-NewCol, Size).
											
processSquare(Elem, Board, Row-Col, Random, Size):-
	Random == 1,
	random(0, 2, NewRandom),
	processSquare2(Elem, Board, Row-Col, NewRandom, Size).

processSquare(_Elem, _Board, _Row-_Col, _Random, _Size).
											
processSquare2(Elem, _Board, _Row-_Col, Random, _Size):-
	Random == 0,
	sum([Elem], #=, 15).

processSquare2(Elem, _Board, _Row-_Col, _Random, _Size):-
	sum([Elem], #=, 0).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% PRINT SECTION %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%												
printLine(0).
printLine(Count):-
	write(' ---'),
	Count2 is Count-1,
	printLine(Count2).

% PRINT ELEMENTS OF THE BOARD
printElem(Piece) :- Piece=0, write('   ').
printElem(Piece) :- Piece=1, write(' * ').	
printElem(Piece) :- Piece=15, write(' # ').
printElem(Piece) :- Piece=10, write(' 0 ').
printElem(Piece) :- Piece=11, write(' 1 ').
printElem(Piece) :- Piece=12, write(' 2 ').
printElem(Piece) :- Piece=13, write(' 3 ').
printElem(Piece) :- Piece=14, write(' 4 ').								
						
printBoard(Board, Size) :- nl, printLine(Size), nl,	printList(Board, Size, Size).						

printList([],Size,_) :-
	write('|'), nl,
	printLine(Size), nl.

printList([H|T],Size, 0) :-
	write('|'), nl,
	printLine(Size), nl,
	printList([H|T],Size,Size).
					
printList([H|T], Size, Count) :-
	NewCount is Count - 1,
	write('|'),
	printElem(H),
	printList(T, Size, NewCount).	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% UTILITIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
length_list(L, Ls) :- length(Ls, L).

insertAtEnd(X,[ ],[X]).
insertAtEnd(X,[H|T],[H|Z]) :- insertAtEnd(X,T,Z). 

getElem(Board, Row-Col, Piece):-
	nth0(Row, Board, ColList),
	nth0(Col, ColList, Piece).

compare(Elem) :- Elem == 15.
compare(Elem) :- Elem == 10.
compare(Elem) :- Elem == 11.
compare(Elem) :- Elem == 12.
compare(Elem) :- Elem == 13.
compare(Elem) :- Elem == 14.

copy(Initial, Final, Size, NewFinal):-
	length(Final, Size), maplist(length_list(Size), Final),
	length(NewFinal, Size), maplist(length_list(Size), NewFinal),
	copyRows(Initial, Final, 0-0, Size, NewFinal).

copyRows([], _Final, _Row-_Col, _Size, _NewFinal).

copyRows([H|T], Final, Row-Col, Size, NewFinal):-
	copyRow(H, Final, Row-Col, Size, NewFinal),
	NewRow is Row + 1,
	copyRows(T, Final, NewRow-0, Size, NewFinal).		

copyRow([], _Final, _Row-_Col, _Size, _NewFinal).
					
copyRow([H|T], Final, Row-Col, Size, NewFinal):-
	compare(H),
	changeBoard(Final, Row-Col, H, NewFinal),
	NewCol is Col + 1,
	copyRow(T, NewFinal, Row-NewCol, Size, NewFinal).	

copyRow([_H|T], Final, Row-Col, Size, NewFinal):-
	NewCol is Col + 1,
	copyRow(T, Final, Row-NewCol, Size, NewFinal).	
					
changeBoard(Board, Row-Col, Piece, NewBoard) :-
	changeBoard2(Board, 0, Col, Piece, Row, NewBoard),!.
					
changeBoard2([Head|Tail], FinalRow, Col , Piece, FinalRow, [NewHead|Tail]):-
	replace(Head, Col, Piece, NewHead).
					
changeBoard2([Head|Tail], Row, Col , Piece, FinalRow, [Head|NewTail]) :- 
	Row \= FinalRow,	
	NewRow is Row+1,
	changeBoard2(Tail, NewRow, Col, Piece, FinalRow, NewTail).
																		   														   
replace([_|Tail], 0, Element, [Element|Tail]).

replace([Head|Tail], Col, Element, [Head|Rest]):-
	Col > 0, NewCol is Col-1,
	replace(Tail, NewCol, Element, Rest), !.
	
replace(L, _, _, L).

reset_timer :- statistics(walltime,_).	
print_time :-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10),
	nl, write('Time: '), write(TS), write('ms'), nl, nl.



