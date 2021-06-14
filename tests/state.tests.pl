:- begin_tests(state).
:- use_module("../src/state").


% piece_at_position/3: piece at position
test(piece_at_position__true) :-

    % List with pieces
    Pieces = [piece(white, tower, 6/3), piece(black, king, 3/3)],

    % State
    state:create_state(Pieces, black, [], none, State),

    % Assert
    state:piece_at_position(State, 6/3, piece(white, tower, 6/3)).


% piece_at_position/3: no piece at position
test(piece_at_position__false) :-

    % List with pieces
    Pieces = [piece(white, tower, 6/3), piece(black, king, 3/3)],

    % State
    state:create_state(Pieces, black, [], none, State),

    % Assert
    state:piece_at_position(State, 8/8, none).


% pieces/2: all pieces should be returned
test(pieces) :-

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
test(color_pieces) :-

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
test(check__true) :-

    % List with pieces
    Pieces = [piece(black, king, 1/1), piece(white, tower, 1/5)],

    % State
    state:create_state(Pieces, black, [], none, State),

    % Assert
    state:check(State, black).


% check/2: white king should not be check
test(check__false) :-

    % List with pieces
    Pieces = [piece(black, king, 1/1), piece(white, king, 1/5)],

    % State
    state:create_state(Pieces, white, [], none, State),

    % Assert
    not(state:check(State, white)).

% checkmate_or_stalemate/1: black king should be checkmate
test(checkmate_or_stalemate__true, [nondet]) :-

    % List with pieces
    Pieces = [piece(white, tower, 2/8), piece(white, tower, 1/7), piece(white, king, 4/1), piece(black, king, 8/8)],

    % State
    state:create_state(Pieces, black, [], none, State),

    % Assert
    state:checkmate_or_stalemate(State).

% checkmate_or_stalemate/1: white king should not be checkmate
test(checkmate_or_stalemate__false, [nondet]) :-

    % List with pieces
    Pieces = [piece(white, tower, 2/8), piece(white, tower, 1/7), piece(white, king, 4/1), piece(black, king, 8/8)],

    % State
    state:create_state(Pieces, white, [], none, State),

    % Assert
    not(state:checkmate_or_stalemate(State)).

:- end_tests(state).