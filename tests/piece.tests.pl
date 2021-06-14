:- begin_tests(piece).
:- use_module("../src/piece").
:- use_module("../src/util/utils").


% row_pieces/3
test(row_pieces, [nondet]) :-
    Piece1 = piece(white, horse, 1/4),
    Piece2 = piece(white, horse, 3/1),
    Piece3 = piece(white, horse, 3/4),
    state:create_state([Piece1, Piece2, Piece3], white, [], none, State),
    state:pieces(State, Pieces),

    % Expected Pieces
    ExpectedPieces = [Piece1, Piece3],

    % Received pieces
    piece:row_pieces(4, Pieces, ReceivedPieces),

    % Assert
    utils:list_equals(ExpectedPieces, ReceivedPieces).


% sorted_pieces/3
test(sorted_pieces, [nondet]) :-
    Piece1 = piece(white, horse, 3/4),
    Piece2 = piece(white, horse, 1/4),
    state:create_state([Piece1, Piece2], white, [], none, State),
    state:pieces(State, Pieces),

    % Expected Pieces
    ExpectedPieces = [Piece2, Piece1],

    % Received pieces
    piece:sorted_pieces(Pieces, ExpectedPieces).

:- end_tests(piece).