:- initialization(main, main).

:- use_module("io/parser").
:- use_module("io/writer").
:- use_module("move").
:- use_module("alphabeta").
:- use_module("state").


main :-
    current_prolog_flag(argv, Args),
    handle_main(Args),
    halt(0).

handle_main([_]) :- % Test Mode

    % Load the data from the stdin stream and parse it.
    phrase_from_stream(parser:parse_state(State), current_input),

    % Get all possible states for the current state
    state:all_possible_states(State, NextStates),
    
    % Print all possible states
    writer:write_states_or_draw(State, NextStates).

handle_main([]) :- % Move Mode

    % Load the data from the stdin stream and parse it.
    phrase_from_stream(parser:parse_state(State), current_input),

    % Extract the player from the state
    state:currentcolor(State, Player),

    % Determin the next best move
    alphabeta:alphabeta(Player, State, 0, 3, -100000, 100000, BestState, _),

    % Write the next state to stdout, or write "DRAW" in case of a stalemate.
    % There is a stalemate when the best state is equal to none
    (
        % Print "DRAW"
        BestState == none, writer:write_draw()
        ;
        % Print the best state
        writer:write_state(BestState)
    ).