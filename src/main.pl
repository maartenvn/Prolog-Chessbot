:- initialization main.
:- use_module("io/parser").
:- use_module("io/writer").
:- use_module("moves").

main(Args):-
    handle_main(Args),
    halt(0).

handle_main([TEST]) :- % Test Mode TODO: must be "TEST"

    % Load the data from the stdin stream and parse it.
    phrase_from_stream(parser:parse_board(Board, StartColor), current_input),

    % Get all possible moves for the given board
    moves:all_possible_moves(StartColor, Board, Moves),

    % Print all possible moves
    writer:write_board_moves(StartColor, Board, Moves).

handle_main([]).