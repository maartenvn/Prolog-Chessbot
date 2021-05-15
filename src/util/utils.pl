:- module(utils, []).


%! between1(+Start, ?Value)
%
%  Value is between Start - 1 and Start + 1
:- table between1/2.
between1(Start, Value) :-
    Minus is Start - 1,
    Plus  is Start + 1,
    between(Minus, Plus, Value).


%! between2(+Start, ?Value)
%
%  Value is between Start - 2 and Start + 2
:- table between2/2.
between2(Start, Value) :-
    Minus is Start - 2,
    Plus  is Start + 2,
    between(Minus, Plus, Value).

%! list_equals(+List1, +List2)
%
%  If 2 lists match, ignoring the order
list_equals([], []).
list_equals([H1 | T1], List2) :- 
    member(H1, List2),
    delete(List2, H1, List3),
    list_equals(T1, List3).