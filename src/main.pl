:- initialization main.
:- use_module("io/parser").

main(_):-
    % Load the data from the stdin stream and parse it into a board.
    phrase_from_stream(parser:parse_board(Board, Rokades, StartColor), current_input),

    write(Board),
    write("\n"),
    write(Rokades),
    write("\n"),
    write(StartColor),

    halt(0).