:- initialization main.
:- [parser].

main(_):-
    % Load the data from the stdin stream and parse it into a board.
    phrase_from_stream(parse_board(Board), current_input),

    write(Board),

    halt(0).