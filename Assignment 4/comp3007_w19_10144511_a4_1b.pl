count([],0).
count([X|T],N) :- ( 
    countZeroes(intToList(X, L), Y) =:= 0 ->
        count(T, N1), N is N1 + 1
    ; countZeroes(intToList(X, L), Y) \= 0 ->
        count(T, N)
).

intToList(0,[]).
intToList(N,[A|As]) :- N1 is floor(N/10), A is N mod 10, intToList(N1, As).

countZeroes([],0).
countZeroes([0|Tail],N) :- countZeroes(Tail,N1), N is N1 + 1.
countZeroes([X|Tail],N) :- X \= 0, countZeroes(Tail,N).

divOperation(B, S, C) :-  B =< X1, C is (S * (X1)).