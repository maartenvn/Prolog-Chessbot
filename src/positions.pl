:- module(positions, []).


%! pawn_start_position(+X/+Y, +Color)
%
%  Pawn is on it's starting position.
pawn_start_position(_/2, white).
pawn_start_position(_/7, black).


%! pawn_promotion_position(+X/+Y, +Color)
%
%  Pawn is on it's promotion position.
pawn_promotion_position(_/8, white).
pawn_promotion_position(_/1, black).


%! pawn_passant_position(+X/+Y, +Color)
%
%  Pawn is on a position where an en-passant is possible.
pawn_passant_position(_/4, white).
pawn_passant_position(_/5, black).


%! forward_position(+X/+Y, +Color, +X/-Y)
% 
%  Forward for a given piece
%  For white pieces: +1
%  For black pieces: -1
forward_position(X/Y, white, X/YNew) :- YNew is Y + 1.
forward_position(X/Y, black, X/YNew) :- YNew is Y - 1.


%! horse_position(+Piece, +Pieces, -Position)
%
%  Move that could be done by the horse from a given piece
horse_position(piece(Color, _, X/Y), Pieces, XPos/YPos) :-

    % TODO: create seperate predicate to use for [N - 1, N + 1]
    XMinus2 is X - 2,
    XPlus2  is X + 2,
    YMinus2 is Y - 2,
    YPlus2  is Y + 2,

    % Positions in a square the current position
    % (X/Y) will also be unified
    between(XMinus2, XPlus2, XPos),
    between(YMinus2, YPlus2, YPos),

    % New position must be valid
    valid_position(XPos/YPos),

    % Position must be empty or taken by an opponent piece
    empty_or_opponent_position(XPos/YPos, Color, Pieces),

    % Difference in positions
    XDiff is X - XPos,
    YDiff is Y - YPos,

    % Possible differences for the move
    PossibleDifferences = [
        (-1, 2),
        (-2, 1),
        (1, 2),
        (2, 1),
        (-2, -1),
        (-1, -2),
        (2, -1),
        (1, -2)
    ],

    % Difference must be a member of the possible differences
    member((XDiff, YDiff), PossibleDifferences).


%! square_position(+Piece, +Pieces, -XPos/-YPos)
%
%  Position in a square around a given piece
square_position(OldPiece, Pieces, XPos/YPos) :-
    OldPiece = piece(Color, _, X/Y),

    % TODO: create seperate predicate to use for [N - 1, N + 1]
    XMinus is X - 1,
    XPlus  is X + 1,
    YMinus is Y - 1,
    YPlus  is Y + 1,

    % Positions in a square the current position
    % (X/Y) will also be unified
    between(XMinus, XPlus, XPos),
    between(YMinus, YPlus, YPos),

    % Position must not be (X/Y)
    XPos/YPos \= X/Y,

    % New position must be valid
    valid_position(XPos/YPos),

    % New position must be empty or taken by an opponent piece
    empty_or_opponent_position(XPos/YPos, Color, Pieces).


%! valid_position(+X/+Y)
%
%  Check if a give coordinate is a valid position on the board for a piece to move to.
%  Will check if the position is on the board (not outside).
%
%  WARNING: This predicate will not check if the position is allowed for the particular piece type!
valid_position(X/Y) :- 

    % X must be inside the board
    between(1, 8, X),
    
    % Y must be inside the board
    between(1, 8, Y).


%! opponent_position(+X/+Y, +Color, +Pieces)
%
%  Check if a given position is taken by a piece of the opponent player.
opponent_position(X/Y, Color, Pieces) :-

    % Opponent color
    opponent(Color, OpponentColor),

    % Piece must be of the opponent's color
    member(piece(OpponentColor, _, X/Y), Pieces).


%! opponent_position(+X/+Y, +Color, +Pieces, -OpponentPiece)
%
%  Check if a given position is taken by a piece of the opponent player.
%  Unify the piece with OpponentPiece.
opponent_position(X/Y, Color, Pieces, OpponentPiece) :-

    % Opponent color
    opponent(Color, OpponentColor),

    % Opponent piece
    OpponentPiece = piece(OpponentColor, _, X/Y),

    % Piece must be of the opponent's color
    member(OpponentPiece, Pieces).


%! empty_position(+X/+Y, +Color, +Pieces)
%
%  Check if a given position is not taken by a piece.
empty_position(X/Y, Pieces) :-
    not(member(piece(_, _, X/Y), Pieces)).


%! empty_or_opponent_position(+X/+Y, +Color, +Pieces)
%
%  Check if a position is empty or taken by a piece of the opponent player.
empty_or_opponent_position(X/Y, Color, Pieces) :- opponent_position(X/Y, Color, Pieces), !.
empty_or_opponent_position(X/Y, _, Pieces) :- empty_position(X/Y, Pieces), !.


%! opponent(+Color, -OpponentColor)
%
%  Opponent color for a given color
%  TODO: move to pieces.pl
opponent(white, black).
opponent(black, white).