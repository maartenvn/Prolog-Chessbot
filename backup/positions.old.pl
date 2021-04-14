%! checkmate(+Board, +Color)
%
% If a king with Color is checkmate.
% The king is is checkmate if it has no possible moves left.
checkmate(Board, Color) :-
    KingPiece = piece(Color, king, Position),

    % Position of the king of the given color
    select(KingPiece, Board, _), !,

    % Opponent player color
    opponent(Color, OpponentColor),

    % Opponent pieces
    player_pieces(OpponentColor, Board, OpponentPieces),

    % Possible moves for the king
    possible_moves(KingPiece, Board, KingMoves),

    checkmate(KingPiece, Board, OpponentPieces, KingMoves).

%! checkmate(+KingPiece, +Board, +OpponentPieces, +Moves)
%
% If the given king is checkmate for the given set of opponent pieces and the given set of moves
checkmate(KingPiece, Board, OpponentPieces, [Move | Moves]) :-
    Move = move(_, _, _, NewPos, _),

    % Do the current move
    do_move(Move, Board, NewBoard),

    % Check if the king is still not safe by doing this move
    not(is_safe(NewPos, NewBoard, OpponentPieces)), !,

    % Recursive call
    checkmate(KingPiece, Board, OpponentPieces, Moves).

checkmate(KingPiece, Board, OpponentPieces, []) :-
    Move = move(_, _, _, NewPos, _),

    % Check if the king is not safe
    not(is_safe(NewPos, Board, OpponentPieces)), !.

%! check(+Board, +Color)
%
% If a king with Color is in-check.
% The king is now in range of attack by the opponent player
check(Board, Color) :-

    % Position of the king of the given color
    select(piece(Color, king, Position), Board, _), !,

    % Opponent player color
    opponent(Color, OpponentColor),

    % Opponent pieces
    player_pieces(OpponentColor, Board, OpponentPieces),

    % Check if any opponent piece can attack the king of the given color
    not(is_safe(Position, Board, OpponentPieces)).


%! is_safe(+Piece, +Board, +OpponentPieces)
%
% If a given position is safe from a potential attack.
is_safe(X/Y, Board, [Piece | Pieces]) :-
    % Destructuring
    % TODO: ask if a @ operator exists like in Haskell
    [OtherPiece | OtherPieces] = Board,

    % Possible move positions for the current piece
    possible_positions(Piece, Board, PiecePositions),

    % Check if the Position is part of the piece possible positions.
    not(member(X/Y, PiecePositions)), !,

    % Recursive call
    is_safe(X/Y, Board, Pieces).

is_safe(_, _, []).


%! player_pieces(+Color, +Board, -PlayerPieces)
%
% Unify all pieces for a given Color from a given Board with BoardPlayer
player_pieces(Color, Board, PlayerPieces) :-
    include(piece_color(Color), Board, PlayerPieces).

% TODO: maybe merge this idk
piece_color(Color, piece(Color, _, _)).


%! do_move(+Move, +Board, -NewBoard)
%
% Update the board with a given move for a given piece.
do_move(move(Color, Type, OldPos, NewPos, _), Board, NewBoard) :-
    % Remove the piece at the old position from the board
    delete(Board, piece(_, _, OldPos), BoardDeleted1),

    % Remove the piece at the new position from the board
    delete(BoardDeleted1, piece(_, _, NewPos), BoardDeleted2),

    % Append the piece at the new position on the board
    append(BoardDeleted2, [piece(Color, Type, NewPos)], NewBoard).


score_move(XPos/YPos, Board, Score) :-
    
    % Color & Type for the piece at position
    select(piece(Color, Type, XPos/YPos), Board, _),

    % Score of the piece.
    score_piece(Color, Type, Score).

% TODO: proper score pieces
score_piece(_, _, 1).

%! all_possible_moves(+Color, +Board, -Moves)
%
% All possible moves for all pieces on the given board of a given color.
all_possible_moves(Color, Board, Moves) :-

    % Get the pieces for the given color
    player_pieces(Color, Board, Pieces),

    % Get all possible moves for the pieces
    all_possible_moves_for_pieces(Pieces, Board, Moves).


%! all_possible_moves_for_pieces(+Pieces, +Board, -Moves)
%
% All possible moves for all given pieces on the given board.
all_possible_moves_for_pieces([Piece | Pieces], Board, Moves) :-

    % All possible moves for the current piece
    possible_moves(Piece, Board, PieceMoves),

    % Recursive call
    all_possible_moves_for_pieces(Pieces, Board, RestMoves),

    % Merge the moves into the moves list
    append(PieceMoves, RestMoves, Moves).

all_possible_moves_for_pieces([], _, []) :- !.


%! possible_moves(+Piece, +Board, -Moves)
%
% All possible moves for the provided piece on the current board.
possible_moves(piece(Color, Type, X/Y), Board, Moves) :-
    Piece = piece(Color, Type, X/Y),

    % All possible positions for a piece
    possible_positions(Piece, Board, NewPositions),

    % Convert all positions to a move
    positions_to_moves(Piece, NewPositions, Moves).


%! positions_to_moves(+Piece, +Positions, -Moves)
%
% Corresponding moves for a given set of positions.
% A "move" contains the position & a potential reward for moving to that position.
positions_to_moves(Piece, [NewPosition | NewPositions], [Move | Moves]) :-
    Piece = piece(Color, Type, OldPosition),

    % Construct the move
    Move = move(Color, Type, OldPosition, NewPosition, 0),
    
    % Recursive Call
    positions_to_moves(Piece, NewPositions, Moves), !.

positions_to_moves(_, [], []) :- !.

%! possible_positions(+Piece, +Board, -Positions)
%
% All possible positions for a specific piece.

% Pawn
possible_positions(piece(Color, pawn, X/Y), Board, Positions) :-
    


% King
possible_positions(piece(Color, king, X/Y), Board, Positions) :-

    % King has same possible positions as square_position
    findall(Position, square_position(X/Y, Color, Board, Position), Positions),
    !.

% Queen
possible_positions(piece(Color, queen, X/Y), Board, Positions) :-

    % Queen has same possible positions as cross_position merged with diagonal_position
    findall(Position, cross_position(X/Y, Color, Board, Position), CrossPositions),
    findall(Position, diagonal_position(X/Y, Color, Board, Position), DiagonalPositions),
    
    % Merge cross positions with the diagonal positions into a full list
    append(CrossPositions, DiagonalPositions, Positions),
    !.

% Tower
possible_positions(piece(Color, tower, X/Y), Board, Positions) :-

    % Tower has same possible positions as cross_position
    findall(Position, cross_position(X/Y, Color, Board, Position), Positions),
    !.
    
% Bishop
possible_positions(piece(Color, tower, X/Y), Board, Positions) :-

    % Bishop has same possible positions as diagonal_position
    findall(Position, diagonal_position(X/Y, Color, Board, Position), Positions),
    !.
    
    
%! valid_position(+X/+Y)
%
% Check if a give coordinate is a valid position on the board for a piece to move to.
% Will check if the position is on the board (not outside).
%
% WARNING: This predicate will not check if the position is allowed for the particular piece type!
valid_position(X/Y) :- 

    % X must be inside the board
    X >= 1, 
    X =< 8, 
    
    % Y must be inside the board
    Y >= 1, 
    Y =< 8.


%! opponent_position(+X/+Y, +Color, +Board)
%
% Check if a given position is taken by a piece of the opponent player.
opponent_position(X/Y, Color, Board) :-

    % Opponent color
    opponent(Color, OpponentColor),

    % Piece must be of the opponent's color
    member(piece(OpponentColor, _, X/Y), Board).


%! empty_position(+X/+Y, +Color, +Board)
%
% Check if a given position is not taken by a piece.
empty_position(X/Y, Board) :-
    not(member(piece(_, _, X/Y), Board)).


%! empty_or_opponent_position(+X/+Y, +Color, +Board)
%
% Check if a position is empty or taken by a piece of the opponent player.
empty_or_opponent_position(X/Y, Color, Board) :- opponent_position(X/Y, Color, Board), !.
empty_or_opponent_position(X/Y, _, Board) :- empty_position(X/Y, Board), !.


%! square_position(+X/+Y, +Color, +Board, -XPos/-YPos)
%
% Positions in a square around (X, Y)
square_position(X/Y, Color, Board, XPos/YPos) :-

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
    empty_or_opponent_position(XPos/YPos, Color, Board).
    


%! cross_position(+X/+Y, +Color, +Board, -XPos/-YPos)
%
% Positions in a cross starting from (X, Y) to the edges of the board

% Right row part
cross_position(X/Y, Color, Board, Pos) :-
    (
        path_positions(X/Y, Color, Board, 1, 0, Positions);   % Right row part
        path_positions(X/Y, Color, Board, -1, 0, Positions);  % Left row part
        path_positions(X/Y, Color, Board, 0, 1, Positions);  % Top column part
        path_positions(X/Y, Color, Board, 0, -1, Positions)  % Bottom column part
    ),

    member(Pos, Positions).


%! diagonal_position(+X/+Y, +Board, -XPos/-YPos)
%
% Positions on the diagonals starting from (X, Y)
diagonal_position(X/Y, Color, Board, Pos) :-
    (
        path_positions(X/Y, Color, Board, 1, 1, Positions);   % Top-right diagonal
        path_positions(X/Y, Color, Board, -1, 1, Positions);  % Top-left diagonal
        path_positions(X/Y, Color, Board, 1, -1, Positions);  % Bottom-right diagonal
        path_positions(X/Y, Color, Board, -1, -1, Positions)  % Bottom-left diagonal
    ),

    member(Pos, Positions).


%! path_positions(+X/+Y, +Color, +Board, +XDirection, +YDirection, -Positions)
%
% Positions on a given path with start position (X, Y) and with incremental addition of (XDirection, YDirection)
% TODO: ask prof about code duplication
path_positions(X/Y, Color, Board, XDirection, YDirection, [XPos/YPos | Positions]) :-
    
    % Unify the new position
    XPos is X + XDirection,
    YPos is Y + YDirection,

    % New position must be valid
    valid_position(XPos/YPos),

    % New position must be empty
    empty_position(XPos/YPos, Board), !,

    % Recursivly extend the diagonal
    path_positions(XPos/YPos, Color, Board, XDirection, YDirection, Positions).

path_positions(X/Y, Color, Board, XDirection, YDirection, [XPos/YPos]) :-
    
    % Unify the new position
    XPos is X + XDirection,
    YPos is Y + YDirection,

    % New position must be valid
    valid_position(XPos/YPos),

    % New position must be taken by the opponent
    opponent_position(XPos/YPos, Color, Board), !.

path_positions(_, _, _, _, _, []) :- !.


%! opponent(+Color, -OpponentColor)
%
% Opponent color for a given color
opponent(white, black).
opponent(black, white).


% ----------------------------------------------------------


minimax(MinMax, Color, Board, BestScore, BestMove) :-

    % All potential moves for a given piece.
    possible_moves(Piece, Board, Moves).

    % Find the best move available.