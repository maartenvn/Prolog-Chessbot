:- use_module("../alphabeta").
:- use_module("../state").


%! benchmark_state(+State)
%
%  Benchmark a given state.
benchmark_state(State) :-
    % Start timer
    statistics(walltime, [_ | [_]]),

    % Benchmark
    alphabeta:alphabeta(black, State, 0, 4, -100000, 100000, _, BestScore),

    % Stop timer
    statistics(walltime, [_ | [ExecutionTime]]),

    % Write results
    write("Best score: "), write(BestScore), nl,
    write("Execution took: "), write(ExecutionTime), write(" ms."), nl.

benchmark :-
    state:create_state([piece(black,tower,1/8),piece(black,horse,2/8),piece(black,bishop,3/8),piece(black,queen,4/8),piece(black,king,5/8),piece(black,bishop,6/8),piece(black,horse,7/8),piece(black,tower,8/8),piece(black,pawn,1/7),piece(black,pawn,2/7),piece(black,pawn,3/7),piece(black,pawn,4/7),piece(black,pawn,5/7),piece(black,pawn,6/7),piece(black,pawn,7/7),piece(black,pawn,8/7),piece(white,pawn,6/3),piece(white,pawn,1/2),piece(white,pawn,2/2),piece(white,pawn,3/2),piece(white,pawn,4/2),piece(white,pawn,5/2),piece(white,pawn,7/2),piece(white,pawn,8/2),piece(white,tower,1/1),piece(white,horse,2/1),piece(white,bishop,3/1),piece(white,queen,4/1),piece(white,king,5/1),piece(white,bishop,6/1),piece(white,horse,7/1),piece(white,tower,8/1)], black, [rokade(black,long),rokade(black,short),rokade(white,long),rokade(white,short)],none, State),
    benchmark_state(State).