:- use_module("../alphabeta").
:- use_module("../state").


%! benchmark_state(+State)
%
%  Benchmark a given state.
benchmark_state(State) :-
    % Start timer
    statistics(walltime, [_ | [_]]),

    % Benchmark
    alphabeta:alphabeta(black, State, 0, 3, -100000, 100000, _, BestScore),

    % Stop timer
    statistics(walltime, [_ | [ExecutionTime]]),

    % Write results
    write("Best score: "), write(BestScore), nl,
    write("Execution took: "), write(ExecutionTime), write(" ms."), nl.

benchmark :-
    State = state(rows(row(none,none,none,piece(white,horse,4/1),none,piece(black,horse,6/1),none,none),row(none,piece(black,tower,2/2),none,piece(black,queen,4/2),none,none,piece(white,pawn,7/2),none),row(none,none,piece(white,bishop,3/3),piece(black,pawn,4/3),none,none,none,none),row(piece(black,king,1/4),none,none,none,none,none,none,none),row(piece(white,pawn,1/5),none,none,none,none,none,piece(black,horse,7/5),none),row(piece(white,pawn,1/6),none,none,none,none,none,none,none),row(none,none,none,piece(black,pawn,4/7),piece(white,king,5/7),none,none,none),row(piece(white,queen,1/8),piece(white,horse,2/8),none,none,piece(black,bishop,5/8),none,none,none)),white,[],none),
    benchmark_state(State).