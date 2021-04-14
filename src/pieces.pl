:- module(pieces, []).

%! color_pieces(+Color, +Board, -ColorPieces)
%
%  Unify all pieces for a given Color from a given Board with BoardPlayer
color_pieces(Color, Pieces, ColorPieces) :-
    include(piece_color(Color), Pieces, ColorPieces).

%! piece_color(-Color, +Piece)
%
%  Extract the color of a piece
%  This can be used as a predicate in predicates like "include"
piece_color(Color, piece(Color, _, _)).


%! row_pieces(+Y, +Pieces, -RowPieces)
%
%  List of pieces for a given row.
row_pieces(Y, [Piece | Pieces], [RowPiece | RowPieces]) :- % Match
    Piece = piece(_, _, _/PieceY),

    % Row numbers must match
    PieceY = Y,

    % Append to the list
    RowPiece = Piece, !,

    % Recursive call
    row_pieces(Y, Pieces, RowPieces), !.

row_pieces(Y, [Piece | Pieces], RowPieces) :- % No match
    Piece = piece(_, _, _/PieceY),
    
    % Row numbers must not match
    PieceY \= Y,

    % Recursive call
    row_pieces(Y, Pieces, RowPieces), !.

row_pieces(_, [], []) :- !.


%! sorted_pieces(+Pieces, -SortedPieces)
%
%  Sorted list for a given list of pieces.
sorted_pieces(Pieces, SortedPieces) :-
    sort(Pieces, SortedPieces).