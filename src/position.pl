:- module(position, []).

:- use_module("state").
:- use_module("piece").
:- use_module("util/utils").


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


%! forward_position(+X/+Y, +Color, +X/-Y)
% 
%  Forward for a given piece
%  For white piece: +1
%  For black piece: -1
forward_position(X/Y, white, X/YNew) :- YNew is Y + 1.
forward_position(X/Y, black, X/YNew) :- YNew is Y - 1.


%! horse_position(+Piece, +State, -Position)
%
%  Move that could be done by the horse from a given piece
horse_position(piece(Color, _, X/Y), State, XPos/YPos) :-

    % Positions in a square the current position
    % (X/Y) will also be unified
    utils:between2(X, XPos),
    utils:between2(Y, YPos),

    % New position must be valid
    valid_position(XPos/YPos),

    % Position must be empty or taken by an opponent piece
    empty_or_opponent_position(XPos/YPos, Color, State),

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
    memberchk((XDiff, YDiff), PossibleDifferences).


%! square_position(+Piece, +State, -XPos/-YPos)
%
%  Position in a square around a given piece
square_position(Piece, State, XPos/YPos) :-
    piece:color(Piece, Color),
    piece:position(Piece, X/Y),

    % Positions in a square the current position
    % (X/Y) will also be unified
    utils:between1(X, XPos),
    utils:between1(Y, YPos),

    % Position must not be (X/Y)
    XPos/YPos \== X/Y,

    % New position must be valid
    valid_position(XPos/YPos),

    % New position must be empty or taken by an opponent piece
    empty_or_opponent_position(XPos/YPos, Color, State).


%! valid_position(+X/+Y)
%
%  Check if a give coordinate is a valid position on the board for a piece to move to.
%  Will check if the position is on the board (not outside).
%
%  WARNING: This predicate will not check if the position is allowed for the particular piece type!
:- table valid_position/1. % Memoization
valid_position(X/Y) :- 

    % X must be inside the board
    between(1, 8, X),
    
    % Y must be inside the board
    between(1, 8, Y).


%! valid_positions(-Positions)
%
%  List of all possible positions on the board
%
%  This could also be done using "findall", but hard-coding this makes it significantly faster when alpha-beta pruning.
:- table valid_positions/1. % Memoization
valid_positions(Positions) :-
    findall(X/Y, valid_position(X/Y), Positions).


%! empty_position(+X/+Y, +State)
%
%  Check if a given position is not taken by a piece.
empty_position(X/Y, State) :-

    % Piece at the given position must be none
    state:piece_at_position(State, X/Y, none).


%! opponent_position/3(+X/+Y, +Color, +State)
%
%  Check if a given position is taken by a piece of the opponent player.
opponent_position(X/Y, Color, State) :-
    opponent_position(X/Y, Color, State, _).


%! opponent_position/4(+X/+Y, +Color, +State, -OpponentPiece)
%
%  Check if a given position is taken by a piece of the opponent player.
%  Unify the piece with OpponentPiece.
opponent_position(X/Y, Color, State, OpponentPiece) :-

    % Opponent color
    piece:opponent(Color, OpponentColor),

    % Piece at the given position must be as described above
    state:piece_at_position(State, X/Y, piece(PieceColor, PieceType, _)),

    % Piece color must match opponent color
    PieceColor == OpponentColor,

    % Opponent Piece
    OpponentPiece = piece(OpponentColor, PieceType, X/Y).


%! empty_or_opponent_position(+X/+Y, +Color, +State)
%
%  Check if a position is empty or taken by a piece of the opponent player.
empty_or_opponent_position(X/Y, _, State) :- empty_position(X/Y, State).
empty_or_opponent_position(X/Y, Color, State) :- opponent_position(X/Y, Color, State).


%! empty_between_positions(+X1/+Y, +X2/+Y, +State)
%
%  If the positions between 2 coordinates (in a row line) are empty
empty_between_positions(X/Y, X/Y, _).              % Base Case
empty_between_positions(X1/Y, X2/Y, _) :-          % No positions between the given positions
    XPlus is X1 + 1,

    % There are no positions between the X1 & X2
    XPlus == X2.
empty_between_positions(X1/Y, X2/Y, State) :-      % Recursion Case
    % X1 must be smaller than X2
    X1 < X2,

    % Position must be between X1 and X2
    XPos is X1 + 1,

    % New position must be empty
    empty_position(XPos/Y, State),

    % Recursive call
    empty_between_positions(XPos/Y, X2/Y, State), !.