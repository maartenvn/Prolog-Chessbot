:- begin_tests(test).
:- use_module(library(lists)).

test(reverse) :-
        reverse([a,b], [b,a]).

:- end_tests(lists).