:- module(pieces, []).

:- use_module("positions").
:- use_module("moves").


%! checkmate/2(+Board, +Color)
%
% If a king with Color is checkmate.
% The king is is checkmate if it has no possible moves left.
checkmate(Board, Color) :-
    Board = board(Pieces, _, _),
    KingPiece = piece(Color, king, _),

    % Position of the king of the given color
    select(KingPiece, Pieces, _), !,

    % Possible moves for the king
    moves:possible_moves(KingPiece, Board, KingMoves),

    % Helper
    checkmate(KingPiece, Board, KingMoves).


%! checkmate/3(+KingPiece, +Board, +Moves)
%
% If the given king is checkmate for the given set of opponent pieces and the given set of moves
checkmate(KingPiece, Board, [Move | Moves]) :-
    KingPiece = piece(Color, king, _),
    NewKingPiece = piece(Color, king, _),

    % Do the current move
    moves:do_move(Move, Board, NewBoard),
    NewBoard = board(NewPieces, _, _),

    % Position of the king after the move
    select(NewKingPiece, NewPieces, _), !,

    % Check if the king is still not safe by doing this move
    not(is_safe(NewKingPiece, NewBoard)), !,

    % Recursive call
    checkmate(KingPiece, Board, Moves), !.

checkmate(_, _, []) :- !.


%! check(+Board, +Color)
%
% If a king with Color is in-check.
% The king is now in range of attack by the opponent player
check(Board, Color) :-
    Board = board(Pieces, _, _),
    KingPiece = piece(Color, king, _),

    % Position of the king of the given color
    select(KingPiece, Pieces, _), !,

    % Check if any opponent piece can attack the king of the given color
    not(is_safe(KingPiece, Board)).


%! is_safe(+Piece, +Board, +OpponentPieces)
%
%  If a given piece is safe from a potential attack.
is_safe(Piece, Board) :-
    Piece = piece(Color, _, _),
    
    % Opponent Color
    positions:opponent(Color, OpponentColor),

    % All possible moves by the opponent
    moves:all_possible_moves(OpponentColor, Board, Moves),

    % Check if the given piece is part of any of the moves
    is_safe_for_moves(Piece, Moves).


%! is_safe_for_moves(+Piece, +Moves)
%
%  if a given piece is safe for a given set of moves.
is_safe_for_moves(Piece, [Move | Moves]) :-
    Move = move(DeletePieces, _, _, _),
    
    % Move must not be part of the pieces to delete
    not(member(Piece, DeletePieces)),

    % Recursive call
    is_safe_for_moves(Piece, Moves), !.
is_safe_for_moves(_, []) :- !.


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