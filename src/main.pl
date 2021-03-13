:- initialization main.

main:-
    read_string(user_input, _, Str),
    write(Str),
    halt(0).

