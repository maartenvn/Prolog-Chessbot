:- initialization main.
:- use_module("io/parser").
:- use_module("io/writer").
:- use_module("moves").
:- use_module("alphabeta").
:- use_module("state").


main(Args):-
    handle_main(Args),
    halt(0).

handle_main([_]) :- % Test Mode TODO: this must parse "TEST"

    % Load the data from the stdin stream and parse it.
    phrase_from_stream(parser:parse_state(State), current_input),

    % Get all possible states for the current state
    moves:all_possible_states(State, NextStates),

    % Print all possible states
    writer:write_states(NextStates).

handle_main([]) :- % Move Mode

    % Load the data from the stdin stream and parse it.
    phrase_from_stream(parser:parse_state(State), current_input),

    % Extract the player from the state
    state:currentcolor(State, Player),

    % Determin the next best move
    alphabeta:alphabeta(Player, State, 5, -100000, 100000, BestState, BestScore),

    %write(BestScore),

    % Print the best state
    writer:write_state(BestState).