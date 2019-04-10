%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Authors:
%       · Alejandro Santorum Varela - alejandro.santorum@estudiante.uam.es
%       · Sergio Galán Martín - sergio.galanm@estudiante.uam.es
%   File: assignment3.pl
%   Date: Apr. 3, 2019
%   Version: 1.0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXERCISE 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
duplica([],[]). % base case

duplica([X|L], [X, X|L1]) :- % first element of L is equal to the first
    duplica(L,L1).           % and second elements of L1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXERCISE 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
concatena([], L, L). % base case

concatena([X|L1], L2, [X|L3]) :- % elements of first lists goes first
    concatena(L1, L2, L3).       % in the concatenated list


invierte([],Z,Z) :- !. % base case

invierte([H|T],Z,Acc) :-    % Inserting first element of the first list
    invierte(T,Z,[H|Acc]).  % at the beginning of the auxiliary list

invierte(L1, L2) :-         % Interface for invierte/3
    invierte(L1, L2, []).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXERCISE 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
palindromo(L) :-
    invierte(L,L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXERCISE 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
divide(L, 0, [], L). % base case, L1 is empty, so the rest of the
                     % elements belogs to L2

divide([F|R1],N,[F|R2],L) :- % iterating over first N elements of L
    M is N-1,
    divide(R1, M, R2, L).    % inserting those N elems into L1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXERCISE 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
aplasta(L, Ret) :-          % interface for aplasta/3
    aplasta(L, [], Ret).

aplasta([], Ret, Ret).      % base case

aplasta([F|R], Aux, Ret) :-
    is_list(F),             % case in which first(L) is a list
    !,
    aplasta(F, Ret2),
    concatena(Aux, Ret2, Concat), % concatenating flatten list with the list got before
    aplasta(R, Concat, Ret).

aplasta([F|R], Aux, Ret) :-   % case in which first(L) is not a list
    concatena(Aux, [F], Concat),
    aplasta(R, Concat, Ret).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXERCISE 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
next_factor(N, F, NF) :- % getting possible next factor
    F is 2,
    NF is 3;
    F =< N, % we had to fix it from documentation
    NF is F+2,
    1 is mod(F,2).

primos(1,[],_,_). % base case

primos(N, [NF|R], F, Init) :-
    next_factor(Init, F, NF), % Initializing factor list if not instanciated
    primos(N, [NF|R], NF, Init).

primos(N, L, F, Init) :-
    next_factor(Init, F, NF), % F does not divide N
    primos(N, L, NF, Init).

primos(N, [F|R], F, Init) :-
    0 is mod(N,F),  % case in which F divides N
    X is div(N,F),
    primos(X, R, F, Init).

primos(N, L) :- primos(N, L, 2, N), !. % interface for primos/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXERCISE 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cod_primero(X,[],[],[X]).

cod_primero(X,[Y|R],[Y|R],[X]) :-
    not(X is Y).

cod_primero(X,[X|R],LRem,[X|R2]) :-
    cod_primero(X,R,LRem,R2).


cod_all([F|R],[E]) :-
    cod_primero(F,R,[],E).

cod_all([F1|R1],[F2|R2]) :-
    cod_primero(F1,R1,Aux,F2), cod_all(Aux,R2).


is_coded([],[0,_]).

is_coded([F1|R1],[F2,F1]) :-
    is_coded(R1,[F3,F1]), F2 is F3+1.


format_list([],[]).

format_list([F1|R1],[F2|R2]) :-
    is_coded(F1,F2),format_list(R1,R2).

run_length(L,L1) :-
    cod_all(L,Aux),format_list(Aux,L1).

%%is_c([X|Y], [L,X]) :- length([X|Y], L). % Santini implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXERCISE 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse(X-Y, X, Y). % parsing element with the form of [a-b] as a and b separately

build_tree([], X) :- % base case: empty list of elements
    X = tree(nil,nil,nil),!.

build_tree([A], X) :- % base case: list with a single element
    parse(A, A1, _),
    X = tree(A1,nil,nil),!.

build_tree([F|R], X) :-
    parse(F, F1, _), % parsing element
    build_tree(R,Z), % calling recursively to keep building the tree
    X = tree(1, tree(F1,nil,nil), Z). % building the tree


encode_elem(X1,[F],T) :- % base case
    T = tree(_, L, _),
    L = tree(X1,_,_),    % element is found at the left tree
    F is 0,!.            % the list must contain a 0

encode_elem(X1,[F],T) :- % base case
    T = tree(_, _, R),
    R = tree(X1,_,_),    % element is found at the last right tree
    F is 1,!.

encode_elem(X1,[1|Rest],T) :-
    T = tree(_, _, R),
    encode_elem(X1,Rest,R). % the element is not at the left tree, nor the right
                            % keep recursively


encode_list([],[],_). % base case

encode_list([Fc|Rc],[Fk|Rk],T) :-
    encode_elem(Fc,Fk,T), % using last predicate as auxiliary
    encode_list(Rc,Rk,T). % carry on recursively


%%%% MAYBE THIS HAS TO BE FIXED %%%%%%
build_alphabet_tree(T) :-
    build_tree([a-0,b-1,c-2,d-3,e-4,f-5,g-6,
                 h-7,i-8,j-9,k-10,l-11,m-12,
                 n-13,o-14,p-15,q-16,r-17,
                 s-18,t-19,u-20,v-21,w-22,
                 x-23,y-24,z-25], T).
encode(L,X) :-
    build_alphabet_tree(T),
    encode(L,X,T).

encode(L,X,T) :-
    encode_list(L,X,T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

freq_elem(Elem, List, Freq) :-
    freq_elem(Elem, List, Freq, Freq),!.

freq_elem(_, [], _, 0).

freq_elem(Elem, [Elem|R], Freq, Actual) :-
    freq_elem(Elem, R, Freq, Next),
    Actual is Next+1.
    
freq_elem(Elem, [_|R], Freq, Actual) :-
    freq_elem(Elem, R, Freq, Actual).

delete_elem(_, [], []).
delete_elem(X, [X|Xs], Y) :-
    delete_elem(X, Xs, Y), !.
delete_elem(X, [T|Xs], [T|Y]) :-
    delete_elem(X, Xs, Y).

frequencies([],[]).
frequencies([Elem|Rest], [LFf|LFr]) :-
    parse(LFf, Elem, Freq),
    freq_elem(Elem, [Elem|Rest], Freq),
    delete_elem(Elem, Rest, Clean),
    frequencies(Clean, LFr).
