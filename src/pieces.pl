:- module(pieces, []).

:- use_module("positions").
:- use_module("moves").

%! position(+Piece, -Position)
%
%  Extract the position from the given piece.
position(piece(_, _, Position), Position).


%! type(+Piece, -Type)
%
%  Extract the type from the given piece.
type(piece(_, Type, _), Type).


%! type(+Color, -Type)
%
%  Extract the color from the given piece.
color(piece(Color, _, _), Color).


%! color_pieces(+Color, +Pieces, -ColorPieces)
%
%  Unify all pieces for a given Color from the given state.
color_pieces(Color, Pieces, ColorPieces) :-

    % Extract the pieces that match the given color.
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


%! checkmate/2(+State, +Color)
%
% If a king with Color is checkmate.
% The king is is checkmate if it has no possible moves left.
checkmate(State, Color) :-
    state:pieces(State, Pieces),
    KingPiece = piece(Color, king, _),

    % Position of the king of the given color
    select(KingPiece, Pieces, _), !,

    % Possible moves for the king
    moves:possible_moves(KingPiece, State, KingMoves),

    % Helper
    checkmate(KingPiece, State, KingMoves).


%! checkmate/3(+KingPiece, +State, +Moves)
%
% If the given king is checkmate for the given set of opponent pieces and the given set of moves
checkmate(KingPiece, State, [Move | Moves]) :-
    KingPiece = piece(Color, king, _),
    NewKingPiece = piece(Color, king, _),

    % Do the current move
    moves:do_move(Move, State, NewState),
    state:pieces(NewState, NewPieces),

    % Position of the king after the move
    select(NewKingPiece, NewPieces, _), !,

    % Check if the king is still not safe by doing this move
    not(is_safe(NewKingPiece, NewState)), !,

    % Recursive call
    checkmate(KingPiece, State, Moves), !.

checkmate(_, _, []) :- !.


%! check(+State, +Color)
%
% If a king with Color is in-check.
% The king is now in range of attack by the opponent player
check(State, Color) :-
    state:pieces(State, Pieces),
    KingPiece = piece(Color, king, _),

    % Position of the king of the given color
    select(KingPiece, Pieces, _), !,

    % Check if any opponent piece can attack the king of the given color
    not(is_safe(KingPiece, State)).


%! is_safe(+Piece, +State, +OpponentPieces)
%
%  If a given piece is safe from a potential attack.
is_safe(Piece, State) :-
    Piece = piece(Color, _, _),
    
    % Opponent Color
    positions:opponent(Color, OpponentColor),

    % All possible moves by the opponent
    moves:all_possible_moves(OpponentColor, State, Moves),

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