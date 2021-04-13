%! all_possible_moves(+Color, +Board, -Moves)
%
%  All possible moves for all pieces on the given board of a given color.
all_possible_moves(Color, Board, Moves) :-

    % Get the pieces for the given color
    player_pieces(Color, Board, Pieces),

    % Get all possible moves for the pieces
    all_possible_moves_for_pieces(Pieces, Board, Moves).


%! all_possible_moves_for_pieces(+Pieces, +Board, -Moves)
%
%  All possible moves for all given pieces on the given board.
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
%  All possible moves for a specific piece.

% King
possible_moves(Piece, Board, Moves) :-
    Piece = piece(_, king, _),

    % King can move in a square
    square_moves(Piece, Board, Moves), !.

% Queen
possible_moves(Piece, Board, Moves) :-
    Piece = piece(_, queen, _),

    % Queen can move diagonally or in a cross
    cross_moves(Piece, Board, CrossMoves),
    diagonal_moves(Piece, Board, DiagonalMoves),

    % Merge possible moves
    append([CrossMoves, DiagonalMoves], Moves), !.

% Tower
possible_moves(Piece, Board, Moves) :-
    Piece = piece(_, tower, _),

    % Tower can move in a cross
    cross_moves(Piece, Board, Moves), !.

% Bishop
possible_moves(Piece, Board, Moves) :-
    Piece = piece(_, bishop, _),

    % Bishop can move in diagonally.
    diagonal_moves(Piece, Board, Moves), !.

% Horse
possible_moves(Piece, Board, Moves) :-
   Piece = piece(_, horse, _),

   % Horse positions
   findall(Position, horse_position(Piece, Board, Position), Positions),

   % Convert positions into moves
   positions_to_moves(Piece, Positions, Moves), !.

% Pawn
possible_moves(Piece, Board, Moves) :-
    Piece = piece(_, pawn, _),

    % Possible moves
    pawn_forward_moves(Piece, Board, ForwardMoves),
    pawn_diagonal_moves(Piece, Board, DiagonalMoves),
    pawn_promotion_moves(Piece, Board, PromitionMoves),
    pawn_passant_moves(Piece, Board, PassantMoves),

    % Merge possible moves
    append([ForwardMoves, DiagonalMoves, PromitionMoves, PassantMoves], Moves), !.


%! pawn_moves(+OldPiece, +Board, +Moves)
% 
%  Moves for the pawn going forward
pawn_forward_moves(Piece, Board, Moves) :- % Pawn on start position (can move 2 steps forward)
    Piece = piece(Color, _, X/Y),

    % Pawn must be on start position
    pawn_start_position(Piece),

    % First position must be valid & empty (otherwise the pawn is not able to move 2 steps forward)
    XNew1 is X,
    forward(Color, Y, YNew1),
    valid_position(XNew1/YNew1),
    empty_position(XNew1/YNew1, Board),

    % Second position must be valid & empty
    XNew2 is X,
    forward(Color, YNew1, YNew2),
    valid_position(XNew2/YNew2),
    empty_position(XNew2/YNew2, Board),
    
    % Create the moves
    create_move(Piece, XNew1/YNew1, Move1),
    create_move(Piece, XNew2/YNew2, Move2),

    % Append the moves to the moves list
    append([[Move1, Move2]], Moves), !.

pawn_forward_moves(Piece, Board, Moves) :- % Pawn (can move max 1 step forward)
    Piece = piece(Color, _, X/Y),

    % First position must be valid & empty
    XNew1 is X,
    forward(Color, Y, YNew1),
    valid_position(XNew1/YNew1),
    empty_position(XNew1/YNew1, Board),

    % Create the moves
    create_move(Piece, XNew1/YNew1, Move1),

    % Append the moves to the moves list
    append([[Move1]], Moves), !.

pawn_forward_moves(_, _, []) :- !. % Pawn cannot move forward


%! pawn_diagonal_moves(+Piece, +Board, -Moves)
%
%  Moves for the given pawn moving diagonally
pawn_diagonal_moves(Piece, Board, Moves) :-
    
    % Moves for both diagonal parts
    pawn_diagonal_moves_part(Piece, Board, -1, LeftMoves),
    pawn_diagonal_moves_part(Piece, Board, 1, RightMoves),

    % Merge the 2 lists
    append([LeftMoves, RightMoves], Moves).


%! pawn_diagonal_moves_part(+Piece, +Board, +XDifference, -Moves)
%
%  Moves for the given pawn moving diagonally either left or right.
%  XDifference = 1: right diagonal move
%  XDifference = -1: left diagonal move
pawn_diagonal_moves_part(Piece, Board, XDifference, Moves) :- % Left diagonal
    Piece = piece(Color, _, X/Y),

    % New position
    XNew is X + XDifference,
    forward(Color, Y, YNew),

    % New position must be valid
    valid_position(XNew/YNew),

    % New position must be taken by an opponent piece
    opponent_position(XNew/YNew, Color, Board),

    % Create the move
    create_move(Piece, XNew/YNew, Move),

    % Append the move to the list
    append([[Move]], Moves), !.
pawn_diagonal_moves_part(_, _, _, []) :- !.


%! pawn_promotion_moves(+Piece, +Board, -Moves)
%
%  Move for the given pawn if reaching a point of promotion
%  TODO: remove board variable
pawn_promotion_moves(Piece, _, Moves) :-
    Piece = piece(Color, _, X/Y),

    % Pawn must be on promotion position
    pawn_promotion_position(Piece),

    % Possible moves
    Moves = [
        move(Piece, piece(Color, queen, X/Y), [], []),
        move(Piece, piece(Color, horse, X/Y), [], []),
        move(Piece, piece(Color, tower, X/Y), [], []),
        move(Piece, piece(Color, bishop, X/Y), [], [])
    ], !.
pawn_promotion_moves(_, _, []) :- !.

%! pawn_passant_moves(+Piece, +Board, -Moves)
%
%  Move for the given pawn if an en-passant move is possible
pawn_passant_moves(Piece, Board, Moves) :-
        
    % En-pasant for both directions
    pawn_passant_moves_part(Piece, Board, -1, LeftMoves),
    pawn_passant_moves_part(Piece, Board, 1, RightMoves),

    % Merge the 2 lists
    append([LeftMoves, RightMoves], Moves).


%! pawn_passant_moves_part(+Piece, +Board, +XDifference, -Moves)
%
%  Moves for the given pawn doing en-passant either left or right
%  XDifference = 1: right en-passant move
%  XDifference = -1: left en-passant move
pawn_passant_moves_part(Piece, Board, XDifference, Moves) :-
    Piece = piece(Color, Type, X/Y),

    % Check if there is an opponent next to the current piece & if it is standing en-passant
    XOpponent is X + XDifference,
    YOpponent is Y,
    opponent_position(XOpponent/YOpponent, Color, Board, OpponentPiece),
    pawn_passant_position(OpponentPiece),

    % New position of the pawn after en-passant
    XNew is X + XDifference,
    forward(Color, Y, YNew),

    % Create the move
    Move = move(Piece, piece(Color, Type, XNew/YNew), [OpponentPiece], []),

    % Append the move to the list
    append([[Move]], Moves), !.
pawn_passant_moves_part(_, _, _, []) :- !.


%! pawn_start_position(+Piece)
%
%  Pawn is on it's starting position.
pawn_start_position(piece(white, pawn, _/2)).
pawn_start_position(piece(black, pawn, _/7)).


%! pawn_promotion_position(+Piece)
%
%  Pawn is on it's promotion position.
pawn_promotion_position(piece(white, pawn, _/8)).
pawn_promotion_position(piece(black, pawn, _/1)).


%! pawn_passant_position(+Piece)
%
%  Pawn is on a position where an en-passant is possible.
pawn_passant_position(piece(white, pawn, _/4)).
pawn_passant_position(piece(black, pawn, _/5)).


%! forward(+Color, +Y, -YNew)
% 
%  Forward for a given piece
%  For white pieces: +1
%  For black pieces: -1
forward(white, Y, YNew) :- YNew is Y + 1.
forward(black, Y, YNew) :- YNew is Y - 1.


%! square_moves(+OldPiece, +Board, -Moves)
%
%  Moves in a square around a given piece
square_moves(OldPiece, Board, Moves) :-

    % Square positions
    findall(Position, square_position(OldPiece, Board, Position), Positions),

    % Convert positions into moves
    positions_to_moves(OldPiece, Positions, Moves).


%! cross_moves(+OldPiece, +Board, -Moves)
%
%  Moves in a cross starting from a given piece
cross_moves(OldPiece, Board, Moves) :-

    path_moves(OldPiece, Board, 1, 0, RightMoves),   % Right row part
    path_moves(OldPiece, Board, -1, 0, LeftMoves),   % Left row part
    path_moves(OldPiece, Board, 0, 1, TopMoves),     % Top column part
    path_moves(OldPiece, Board, 0, -1, BottomMoves), % Bottom column part

    % Merge lists
    append([RightMoves, LeftMoves, TopMoves, BottomMoves], Moves).


%! diagonal_moves(+OldPiece, +Board, -Moves)
%
%  Moves on the diagonals starting from a given piece
diagonal_moves(OldPiece, Board, Moves) :-
    path_moves(OldPiece, Board, 1, 1, TopRightMoves),      % Top-right diagonal
    path_moves(OldPiece, Board, -1, 1, TopLeftMoves),      % Top-right diagonal
    path_moves(OldPiece, Board, 1, -1, BottomRightMoves),  % Bottom-right diagonal
    path_moves(OldPiece, Board, -1, -1, BottomLeftMoves),  % Bottom-left diagonal

    % Merge lists
    append([TopRightMoves, TopLeftMoves, BottomRightMoves, BottomLeftMoves], Moves).


%! positions_to_moves(+OldPiece, +Positions, -Moves)
%
%  Corresponding moves for a given set of positions
positions_to_moves(OldPiece, [Position | Positions], [Move | Moves]) :-

    % Construct the move
    create_move(OldPiece, Position, Move),
    
    % Recursive Call
    positions_to_moves(OldPiece, Positions, Moves), !.
positions_to_moves(_, [], []) :- !.


%! create_move(+OldPiece, +XNew/YNew, -Move)
%
%  Create a move for a given piece and position
%
%  TODO: this name is not very prolog (create_move is not a fact?)
create_move(OldPiece, XNew/YNew, Move) :-
    OldPiece = piece(Color, Type, _),
    NewPiece = piece(Color, Type, XNew/YNew),

    % Unify the move
    Move = move(OldPiece, NewPiece, [], []).


%! path_moves(+Piece, +Board, +XDirection, +YDirection, -Moves)
%
%  Moves on a given path starting from a piece and with incremental addition of (XDirection, YDirection)
%  Will stop the path when a new position is either invalid or blocked by another piece
%
%  TODO: ask prof about code duplication
path_moves(OldPiece, Board, XDirection, YDirection, [Move | Moves]) :-
    OldPiece = piece(Color, Type, X/Y),
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Unify the new piece
    NewPiece = piece(Color, Type, XNew, YNew),

    % Create the move
    create_move(OldPiece, XNew/YNew, Move),

    % New position must be valid
    valid_position(XNew/YNew),

    % New position must be empty
    empty_position(XNew/YNew, Board), !,

    % Recursivly extend the diagonal
    path_moves(NewPiece, Board, XDirection, YDirection, Moves).

path_moves(OldPiece, Board, XDirection, YDirection, [Move]) :-
    OldPiece = piece(Color, _, X/Y),
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Create the move
    create_move(OldPiece, XNew/YNew, Move),

    % New position must be valid
    valid_position(XNew/YNew),

    % New position must be taken by the opponent
    opponent_position(XNew/YNew, Color, Board), !.

path_moves(_, _, _, _, []) :- !.


%! horse_position(+Piece, +Board, -Position)
%
%  Move that could be done by the horse from a given piece
horse_position(piece(Color, _, X/Y), Board, XPos/YPos) :-

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
    empty_or_opponent_position(XPos/YPos, Color, Board),

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

%! square_position(+Piece, +Board, -XPos/-YPos)
%
%  Position in a square around a given piece
square_position(OldPiece, Board, XPos/YPos) :-
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
    empty_or_opponent_position(XPos/YPos, Color, Board).


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


%! opponent_position(+X/+Y, +Color, +Board)
%
%  Check if a given position is taken by a piece of the opponent player.
opponent_position(X/Y, Color, Board) :-

    % Opponent color
    opponent(Color, OpponentColor),

    % Piece must be of the opponent's color
    member(piece(OpponentColor, _, X/Y), Board).


%! opponent_position(+X/+Y, +Color, +Board, -OpponentPiece)
%
%  Check if a given position is taken by a piece of the opponent player.
%  Unify the piece with OpponentPiece.
opponent_position(X/Y, Color, Board, OpponentPiece) :-

    % Opponent color
    opponent(Color, OpponentColor),

    % Opponent piece
    OpponentPiece = piece(OpponentColor, _, X/Y),

    % Piece must be of the opponent's color
    member(OpponentPiece, Board).


%! empty_position(+X/+Y, +Color, +Board)
%
%  Check if a given position is not taken by a piece.
empty_position(X/Y, Board) :-
    not(member(piece(_, _, X/Y), Board)).


%! empty_or_opponent_position(+X/+Y, +Color, +Board)
%
%  Check if a position is empty or taken by a piece of the opponent player.
empty_or_opponent_position(X/Y, Color, Board) :- opponent_position(X/Y, Color, Board), !.
empty_or_opponent_position(X/Y, _, Board) :- empty_position(X/Y, Board), !.


%! opponent(+Color, -OpponentColor)
%
%  Opponent color for a given color
opponent(white, black).
opponent(black, white).


%! player_pieces(+Color, +Board, -PlayerPieces)
%
% Unify all pieces for a given Color from a given Board with BoardPlayer
player_pieces(Color, Board, PlayerPieces) :-
    include(piece_color(Color), Board, PlayerPieces).

% TODO: maybe merge this idk
piece_color(Color, piece(Color, _, _)).