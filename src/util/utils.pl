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