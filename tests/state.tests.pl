:- begin_tests(state).
:- use_module("../src/state").


% piece_at_position/3: piece at position
test(piece_at_position__true, [nondet]) :-

    % List with pieces
    Pieces = [piece(white, tower, 6/3), piece(black, king, 3/3)],

    % State
    state:create_state(Pieces, black, [], none, State),

    % Assert
    state:piece_at_position(State, 6/3, piece(white, tower, 6/3)).


% piece_at_position/3: no piece at position
test(piece_at_position__false, [nondet]) :-

    % List with pieces
    Pieces = [piece(white, tower, 6/3), piece(black, king, 3/3)],

    % State
    state:create_state(Pieces, black, [], none, State),

    % Assert
    state:piece_at_position(State, 8/8, none).


% pieces/2: all pieces should be returned
test(pieces, [nondet]) :-

    % List with pieces
    Pieces = [piece(black, king, 1/1), piece(white, tower, 4/5), piece(black, king, 3/3), piece(white, tower, 1/5)],

    % State
    state:create_state(Pieces, black, [], none, State),

    % Assert
    state:pieces(State, ResultPieces),
    memberchk(piece(black, king, 1/1), ResultPieces),
    memberchk(piece(white, tower, 4/5), ResultPieces),
    memberchk(piece(black, king, 3/3), ResultPieces),
    memberchk(piece(white, tower, 1/5), ResultPieces),
    length(ResultPieces, 4).


% color_pieces/3: all white pieces should be returned
test(color_pieces, [nondet]) :-

    % List with pieces
    Pieces = [piece(black, king, 1/1), piece(white, tower, 4/5), piece(black, king, 3/3), piece(white, tower, 1/5)],

    % State
    state:create_state(Pieces, black, [], none, State),

    % Assert
    state:color_pieces(State, white, ColorPieces),
    memberchk(piece(white, tower, 4/5), ColorPieces),
    memberchk(piece(white, tower, 1/5), ColorPieces),
    length(ColorPieces, 2).


% check/2: black king should be check
test(check__true, [nondet]) :-

    % List with pieces
    Pieces = [piece(black, king, 1/1), piece(white, tower, 1/5)],

    % State
    state:create_state(Pieces, black, [], none, State),

    % Assert
    state:check(State, black).


% check/2: white king should not be check
test(check__false, [nondet]) :-

    % List with pieces
    Pieces = [piece(black, king, 1/1), piece(white, king, 1/5)],

    % State
    state:create_state(Pieces, white, [], none, State),

    % Assert
    not(state:check(State, white)).

:- end_tests(state).