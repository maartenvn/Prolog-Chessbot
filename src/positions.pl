%! possible_moves(+Piece, +Board, -Moves)
%
% All possible moves for the provided piece on the current board.
possible_moves(piece(Color, Type, X/Y), Board, Moves) :-
    Piece = piece(Color, Type, X/Y),

    % All possible positions for a piece
    possible_positions(Piece, Board, Positions),

    % Convert all positions to a move
    positions_to_moves(Color, Type, Positions, Moves).


%! positions_to_moves(+Color, +Type, +Positions, -Moves)
%
% Corresponding moves for a given set of positions.
% A "move" is a potential "piece" on the board.
positions_to_moves(_, _, [], []) :- !.
positions_to_moves(Color, Type, [Position | Positions], [Move | Moves]) :-
    Move = piece(Color, Type, Position),
    
    positions_to_moves(Color, Type,  Positions,  Moves).
    

%! possible_positions(+Piece, +Board, -Positions)
%
% All possible positions for a specific piece.

% King
possible_positions(piece(_, king, X/Y), Board, Positions) :-

    % King has same possible positions as square_position
    findall(Position, square_position(X/Y, Board, Position), Positions),
    !.

% Queen
possible_positions(piece(_, queen, X/Y), Board, Positions) :-

    % Queen has same possible positions as cross_position merged with diagonal_position
    findall(Position, cross_position(X/Y, Board, Position), CrossPositions),
    findall(Position, diagonal_position(X/Y, Board, Position), DiagonalPositions),
    
    % Merge cross positions with the diagonal positions into a full list
    append(CrossPositions, DiagonalPositions, Positions),
    !.

% Tower
possible_positions(piece(_, tower, X/Y), Board, Positions) :-

    % Tower has same possible positions as cross_position
    findall(Position, cross_position(X/Y, Board, Position), Positions),
    !.
    
% Bishop
possible_positions(piece(_, tower, X/Y), Board, Positions) :-

    % Bishop has same possible positions as diagonal_position
    findall(Position, diagonal_position(X/Y, Board, Position), Positions),
    !.
    
    
%! valid_position(+X/+Y, +Board)
%
% Check if a give coordinate is a valid position on the board for a piece to move to.
% warning: This predicate will not check if the position is allowed for the particular piece type!
%
% Will check for:
%   * If the position is on the board (not outside)
%   * If the position is taken by another piece
valid_position(X/Y, Board) :- 

    % X must be inside the board
    X >= 1, 
    X =< 8, 
    
    % Y must be inside the board
    Y >= 1, 
    Y =< 8,

    % Position must not be taken by another piece
    not(member(piece(_, _, X/Y), Board)).


%! square_position(+X/+Y, +Board, -XPos/-YPos)
%
% Positions in a square around (X, Y)
square_position(X/Y, Board, XPos/YPos) :-

    % TODO: create seperate predicate to use for [N - 1, N + 1]
    XMinus is X - 1,
    XPlus  is X + 1,
    YMinus is Y - 1,
    YPlus  is Y + 1,

    % Positions in a square the current position
    % (XPos, YPos) will also be unified, but filtered by valid_position
    between(XMinus, XPlus, XPos),
    between(YMinus, YPlus, YPos),

    % New position must be valid
    valid_position(XPos/YPos, Board).


%! cross_position(+X/+Y, +Board, -XPos/-YPos)
%
% Positions in a cross starting from (X, Y) to the edges of the board

% Right row part
cross_position(X/Y, Board, Pos) :-
    (
        path_positions(X/Y, Board, 1, 0, Positions);   % Right row part
        path_positions(X/Y, Board, -1, 0, Positions);  % Left row part
        path_positions(X/Y, Board, 0, 1, Positions);  % Top column part
        path_positions(X/Y, Board, 0, -1, Positions)  % Bottom column part
    ),

    member(Pos, Positions).


%! diagonal_position(+X/+Y, +Board, -XPos/-YPos)
%
% Positions on the diagonals starting from (X, Y)
diagonal_position(X/Y, Board, Pos) :-
    (
        path_positions(X/Y, Board, 1, 1, Positions);   % Top-right diagonal
        path_positions(X/Y, Board, -1, 1, Positions);  % Top-left diagonal
        path_positions(X/Y, Board, 1, -1, Positions);  % Bottom-right diagonal
        path_positions(X/Y, Board, -1, -1, Positions)  % Bottom-left diagonal
    ),

    member(Pos, Positions).


%! path_positions(+X/+Y, +Board, +XDirection, +YDirection, -Positions)
%
% Positions on a given path with start postion (X, Y) and with incremental addition of (XDirection, YDirection)
path_positions(X/Y, Board, XDirection, YDirection, [XPos/YPos | Positions]) :-
    
    % Unify the new position
    XPos is X + XDirection,
    YPos is Y + YDirection,

    % New position must be valid
    valid_position(XPos/YPos, Board), !,

    % Recursivly extend the diagonal
    path_positions(XPos/YPos, Board, XDirection, YDirection, Positions).

path_positions(_X/_Y, _Board, _XDirection, _YDirection, []) :- !.