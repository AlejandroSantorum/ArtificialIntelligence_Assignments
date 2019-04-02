concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).
duplica([],[]).
duplica([X|L], [X, X|L1]) :- duplica(L,L1).
%last(X,X).
%last([X|Xs],L) :- last(L,Xs).
%invierte([],Z,Z) :- !.
%invierte([H|T],Z,Acc) :- invierte(T,Z,[H|Acc]).
%invierte(L1, L2) :- invierte(L1, L2, []).
invierte(Xs, Ys) :- invierte(Xs, [], Ys, Ys).
invierte([], Ys, Ys, []).
invierte([X|Xs], Rs, Ys, [_|Bound]) :- invierte(Xs, [X|Rs], Ys, Bound).
palindromo(L) :- invierte(L,L).
divide(L, 0, [], L).
divide([F|R1],N,[F|R2],L) :- M is N-1, divide(R1, M, R2, L).
aplasta([], []).
aplasta([F1|F1], L) :- is_list(F1), aplasta(F1, L). %% El segundo F1 está mal
aplasta([F|R1], [F|R2]) :- aplasta(R1, R2).
next_factor(N, F, NF) :- F is 2, NF is 3;F < sqrt(N), NF is F+2, 1 is mod(F,2).
primos(1,R,_,_):- R = [].
primos(N, [F|R], F, Init) :- X is N/F, Z is mod(N,F), 0 is Z, primos(X, R, F, Init).
primos(N, L, F, Init) :-  next_factor(Init, F, NF), primos(N, L, NF, Init).
primos(N, L) :- primos(N, L, 2, N).
