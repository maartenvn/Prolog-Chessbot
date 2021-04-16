:- initialization main.
:- use_module("io/parser").
:- use_module("io/writer").
:- use_module("moves").
:- use_module("alphabeta").
:- use_module("state").

main(Args):-
    handle_main(Args),
    halt(0).

handle_main([TEST]) :- % Test Mode TODO: must be "TEST"

    % Load the data from the stdin stream and parse it.
    phrase_from_stream(parser:parse_state(State), current_input),

    % Get all possible moves for the given state
    moves:all_possible_moves(State, Moves),

    % Print all possible moves
    writer:write_state_moves(State, Moves).

handle_main([]) :- % Move Mode

    % Load the data from the stdin stream and parse it.
    phrase_from_stream(parser:parse_state(State), current_input),

    % Extract the player from the state
    state:currentcolor(State, Player),

    % Determin the next best move
    alphabeta:alphabeta(Player, State, 4, -100000, 100000, BestState, BestScore),

    %write(BestScore),

    % Print the best state
    writer:write_state(BestState).