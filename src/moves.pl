:- module(moves, []).

:- use_module("positions").
:- use_module("pieces").
:- use_module("util").

%! do_move(+Move, +Board, -NewBoard)
%
% Update the board with a given move for a given piece.
do_move(move(DeletePieces, AppendPieces, DeleteRokades, NewPassant), Board, NewBoard) :-
    Board = board(OldPieces, OldRokades, _),
    NewBoard = board(NewPieces, NewRokades, NewPassant),

    % Delete pieces
    util:delete_list(OldPieces, DeletePieces, NewPiecesDeleted),

    % Append pieces
    append(NewPiecesDeleted, AppendPieces, NewPieces),

    % Delete Rokades
    util:delete_list(OldRokades, DeleteRokades, NewRokades).


%! all_possible_moves(+Color, +Board, -Moves)
%
%  All possible moves for all pieces on the given board of a given color.
all_possible_moves(Color, Board, Moves) :-
    Board = board(Pieces, _, _),

    % Get the pieces for the given color
    pieces:color_pieces(Color, Pieces, ColorPieces),

    % Get all possible moves for the pieces
    all_possible_moves_for_pieces(ColorPieces, Board, Moves).


%! all_possible_moves_for_pieces(+Pieces, +Board, -Moves)
%
%  All possible moves for all given pieces on the given board.
all_possible_moves_for_pieces([Piece | Pieces], Board, Moves) :-

    % All possible moves for the current piece
    possible_moves(Piece, Board, PieceMoves),

    % Recursive call
    all_possible_moves_for_pieces(Pieces, Board, RestMoves),

    % Merge the moves into the moves list
    append(PieceMoves, RestMoves, Moves), !.
all_possible_moves_for_pieces([], _, []) :- !.


%! possible_moves(+Piece, +Board, -Moves)
%
%  All possible moves for a specific piece.

% King
possible_moves(Piece, board(Pieces, _, _), Moves) :-
    Piece = piece(_, king, _),

    % King can move in a square
    square_moves(Piece, Pieces, Moves), !.

% Queen
possible_moves(Piece, board(Pieces, _, _), Moves) :-
    Piece = piece(_, queen, _),

    % Queen can move diagonally or in a cross
    cross_moves(Piece, Pieces, CrossMoves),
    diagonal_moves(Piece, Pieces, DiagonalMoves),

    % Merge possible moves
    append([CrossMoves, DiagonalMoves], Moves), !.

% Tower
possible_moves(Piece, board(Pieces, _, _), Moves) :-
    Piece = piece(_, tower, _),

    % Tower can move in a cross
    cross_moves(Piece, Pieces, Moves), !.

% Bishop
possible_moves(Piece, board(Pieces, _, _), Moves) :-
    Piece = piece(_, bishop, _),

    % Bishop can move in diagonally.
    diagonal_moves(Piece, Pieces, Moves), !.

% Horse
possible_moves(Piece, board(Pieces, _, _), Moves) :-
   Piece = piece(_, horse, _),

   % Horse positions
   findall(Position, positions:horse_position(Piece, Pieces, Position), Positions),

   % Convert positions into moves
   positions_to_moves(Piece, Pieces, Positions, Moves), !.

% Pawn
possible_moves(Piece, board(Pieces, _, Passant), Moves) :-
    Piece = piece(_, pawn, _),

    % Possible moves
    pawn_forward_moves(Piece, Pieces, ForwardMoves),
    pawn_diagonal_moves(Piece, Pieces, DiagonalMoves),
    pawn_promotion_moves(Piece, Pieces, PromitionMoves),
    pawn_passant_moves(Piece, Pieces, Passant, PassantMoves),

    % Merge possible moves
    append([ForwardMoves, DiagonalMoves, PromitionMoves, PassantMoves], Moves), !.


%! pawn_moves(+OldPiece, +Pieces, -Moves)
% 
%  Moves for the pawn going forward
pawn_forward_moves(Piece, Pieces, Moves) :- % Pawn on start position (can move 2 steps forward)
    Piece = piece(Color, _, X/Y),

    % Pawn must be on start position
    positions:pawn_start_position(X/Y, Color),

    % First position must be valid & empty (otherwise the pawn is not able to move 2 steps forward)
    positions:forward_position(X/Y, Color, XNew1/YNew1),
    positions:valid_position(XNew1/YNew1),
    positions:empty_position(XNew1/YNew1, Pieces),

    % Second position must be valid & empty
    positions:forward_position(XNew1/YNew1, Color, XNew2/YNew2),
    positions:valid_position(XNew2/YNew2),
    positions:empty_position(XNew2/YNew2, Pieces),

    % Create the moves
    create_move(Piece, XNew1/YNew1, Pieces, Move1), % En-passant possibility
    create_move(Piece, XNew2/YNew2, Pieces, [], passant(Color, XNew1/YNew1), Move2),

    % Append the moves to the moves list
    append([[Move1, Move2]], Moves), !.

pawn_forward_moves(Piece, Pieces, Moves) :- % Pawn (can move max 1 step forward)
    Piece = piece(Color, _, X/Y),

    % First position must be valid & empty
    positions:forward_position(X/Y, Color, XNew1/YNew1),
    positions:valid_position(XNew1/YNew1),
    positions:empty_position(XNew1/YNew1, Pieces),

    % Create the moves
    create_move(Piece, XNew1/YNew1, Pieces, Move1),

    % Append the moves to the moves list
    append([[Move1]], Moves), !.

pawn_forward_moves(_, _, []) :- !. % Pawn cannot move forward


%! pawn_diagonal_moves(+OldPiece, +Pieces, -Moves)
%
%  Moves for the given pawn moving diagonally
pawn_diagonal_moves(OldPiece, Pieces, Moves) :-
    
    % Moves for both diagonal parts
    pawn_diagonal_moves_part(OldPiece, Pieces, -1, LeftMoves),
    pawn_diagonal_moves_part(OldPiece, Pieces, 1, RightMoves),

    % Merge the 2 lists
    append([LeftMoves, RightMoves], Moves).


%! pawn_diagonal_moves_part(+OldPiece, +Pieces, +XDifference, -Moves)
%
%  Moves for the given pawn moving diagonally either left or right.
%  XDifference = 1: right diagonal move
%  XDifference = -1: left diagonal move
pawn_diagonal_moves_part(OldPiece, Pieces, XDifference, Moves) :- % Left diagonal
    OldPiece = piece(Color, _, X/Y),

    % New position
    XNew is X + XDifference,
    positions:forward_position(XNew/Y, Color, XNew/YNew),

    % New position must be valid
    positions:valid_position(XNew/YNew),

    % New position must be taken by an opponent piece
    positions:opponent_position(XNew/YNew, Color, Pieces),

    % Create the move
    create_move(OldPiece, XNew/YNew, Pieces, Move),

    % Append the move to the list
    append([[Move]], Moves), !.
pawn_diagonal_moves_part(_, _, _, []) :- !.


%! pawn_promotion_moves(+OldPiece, -Moves)
%
%  Move for the given pawn if reaching a point of promotion
pawn_promotion_moves(OldPiece, Moves) :-
    OldPiece = piece(Color, _, X/Y),

    % Pawn must be on promotion position
    positions:pawn_promotion_position(X/Y, Color),

    % Possible moves
    Moves = [
        move([Piece], [piece(Color, queen, X/Y)], [], none),
        move([Piece], [piece(Color, horse, X/Y)], [], none),
        move([Piece], [piece(Color, tower, X/Y)], [], none),
        move([Piece], [piece(Color, bishop, X/Y)], [], none)
    ], !.
pawn_promotion_moves(_, _, []) :- !.


%! pawn_passant_moves(+Piece, +Pieces, -Moves)
%
%  Move for the given pawn if an en-passant move is possible
pawn_passant_moves(Piece, Pieces, Passant, Moves) :-
    Piece   = piece(PieceColor, pawn, _),
    Passant = passant(PassantColor, _),
    
    % En-passant position must be for the opponent
    positions:opponent(PieceColor, PassantColor),
        
    % En-pasant for both directions
    pawn_passant_moves_part(Piece, Pieces, Passant, -1, LeftMoves),
    pawn_passant_moves_part(Piece, Pieces, Passant, 1, RightMoves),

    % Merge the 2 lists
    append([LeftMoves, RightMoves], Moves), !.
pawn_passant_moves(_, _, _, []) :- !.


%! pawn_passant_moves_part(+Piece, +Pieces, +Passant, +XDifference, -Moves)
%
%  Moves for the given pawn doing en-passant either left or right
%  XDifference = 1: right en-passant move
%  XDifference = -1: left en-passant move
%
%  TODO: List of moves to single move (because a list is useless here)
pawn_passant_moves_part(Piece, Pieces, Passant, XDifference, [Move]) :-
    Piece = piece(Color, pawn, X/Y),
    Passant = passant(PassantColor, XPassant/YPassant),

    % Check if the passant possibility is next to piece.
    XPassant is X + XDifference, 
    positions:forward_position(XPassant/Y, Color, XPassant/YPassant),

    % New position of the pawn after en-passant
    positions:forward_position(XPassant/Y, Color, XNew/YNew),

    % Piece to remove by doing the en-passant move
    OpponentPiece = piece(PassantColor, pawn, XPassant/Y),

    % Create the move
    Move = move([Piece, OpponentPiece], [piece(Color, pawn, XNew/YNew)], [], none), !.
pawn_passant_moves_part(_, _, _, _, []) :- !.


%! square_moves(+OldPiece, +Pieces, -Moves)
%
%  Moves in a square around a given piece
square_moves(OldPiece, Pieces, Moves) :-

    % Square positions
    findall(Position, positions:square_position(OldPiece, Pieces, Position), Positions),

    % Convert positions into moves
    positions_to_moves(OldPiece, Pieces, Positions, Moves).


%! cross_moves(+OldPiece, +Pieces, -Moves)
%
%  Moves in a cross starting from a given piece
cross_moves(OldPiece, Pieces, Moves) :-

    path_moves(OldPiece, Pieces, 1, 0, RightMoves),   % Right row part
    path_moves(OldPiece, Pieces, -1, 0, LeftMoves),   % Left row part
    path_moves(OldPiece, Pieces, 0, 1, TopMoves),     % Top column part
    path_moves(OldPiece, Pieces, 0, -1, BottomMoves), % Bottom column part

    % Merge lists
    append([RightMoves, LeftMoves, TopMoves, BottomMoves], Moves).


%! diagonal_moves(+OldPiece, +Pieces, -Moves)
%
%  Moves on the diagonals starting from a given piece
diagonal_moves(OldPiece, Pieces, Moves) :-
    path_moves(OldPiece, Pieces, 1, 1, TopRightMoves),      % Top-right diagonal
    path_moves(OldPiece, Pieces, -1, 1, TopLeftMoves),      % Top-right diagonal
    path_moves(OldPiece, Pieces, 1, -1, BottomRightMoves),  % Bottom-right diagonal
    path_moves(OldPiece, Pieces, -1, -1, BottomLeftMoves),  % Bottom-left diagonal

    % Merge lists
    append([TopRightMoves, TopLeftMoves, BottomRightMoves, BottomLeftMoves], Moves).


%! positions_to_moves(+OldPiece, +Pieces, +Positions, -Moves)
%
%  Corresponding moves for a given set of positions
positions_to_moves(OldPiece, Pieces, [Position | Positions], [Move | Moves]) :-

    % Construct the move
    create_move(OldPiece, Position, Pieces, Move),
    
    % Recursive Call
    positions_to_moves(OldPiece, Pieces, Positions, Moves), !.
positions_to_moves(_, _, [], []) :- !.


%! create_move/4(+OldPiece, +XNew/YNew, +Pieces, -Move)
%
%  Create a move for a given piece and position
%
%  TODO: this name is not very prolog (create_move is not a fact?)
create_move(OldPiece, XNew/YNew, Pieces, Move) :-
    create_move(OldPiece, XNew/YNew, Pieces, [], none, Move).


%! create_move/6(+OldPiece, +XNew/YNew, +Pieces, +DeleteRokades, +Passant -Move)
%
%  Create a move for a given piece, position, rokades to delete and en-passant possability
create_move(OldPiece, XNew/YNew, Pieces, DeleteRokades, Passant, Move) :- % Opponent on new position
    OldPiece = piece(Color, Type, _),
    NewPiece = piece(Color, Type, XNew/YNew),

    % Opponent at the new position
    positions:opponent_position(XNew/YNew, Color, Pieces, OpponentPiece),

    % Unify the move
    Move = move([OldPiece, OpponentPiece], [NewPiece], DeleteRokades, Passant), !.

create_move(OldPiece, XNew/YNew, Pieces, DeleteRokades, Passant, Move) :- % No piece on new position
    OldPiece = piece(Color, Type, _),
    NewPiece = piece(Color, Type, XNew/YNew),

    % Opponent at the new position
    positions:empty_position(XNew/YNew, Pieces),

    % Unify the move
    Move = move([OldPiece], [NewPiece], DeleteRokades, Passant), !.


%! path_moves(+Piece, +Pieces, +XDirection, +YDirection, -Moves)
%
%  Moves on a given path starting from a piece and with incremental addition of (XDirection, YDirection)
%  Will stop the path when a new position is either invalid or blocked by another piece
%
%  TODO: ask prof about code duplication
path_moves(OldPiece, Pieces, XDirection, YDirection, [Move | Moves]) :-
    OldPiece = piece(Color, Type, X/Y),
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Unify the new piece
    NewPiece = piece(Color, Type, XNew, YNew),

    % Create the move
    create_move(OldPiece, XNew/YNew, Pieces, Move),

    % New position must be valid
    positions:valid_position(XNew/YNew),

    % New position must be empty
    positions:empty_position(XNew/YNew, Pieces), !,

    % Recursivly extend the diagonal
    path_moves(NewPiece, Pieces, XDirection, YDirection, Moves).

path_moves(OldPiece, Pieces, XDirection, YDirection, [Move]) :-
    OldPiece = piece(Color, _, X/Y),
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Create the move
    create_move(OldPiece, XNew/YNew, Pieces, Move),

    % New position must be valid
    positions:valid_position(XNew/YNew),

    % New position must be taken by the opponent
    positions:opponent_position(XNew/YNew, Color, Pieces), !.

path_moves(_, _, _, _, []) :- !.