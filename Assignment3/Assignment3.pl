concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).
duplica([],[]).
duplica([X|L], [X, X|L1]) :- duplica(L,L1).
%last(X,X).
%last([X|Xs],L) :- last(L,Xs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%invierte([],[]).
%invierte([X],[X]).
%invierte([X, Y],[Y, X]).
%invierte([H|T],L) :- concatena(R, [H], L), invierte(T,R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
invierte_aux([],Z,Z) :- !.
invierte_aux([H|T],Z,Acc) :- invierte_aux(T,Z,[H|Acc]).
invierte(L1, L2) :- invierte_aux(L1, L2, []).