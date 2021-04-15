:- module(moves, []).

:- use_module("state").
:- use_module("positions").
:- use_module("pieces").
:- use_module("util").

%! all_possible_boards(+CurrentBoard, +Moves, +NewBoards)
%
%  Generate a new board for every move and append it to a list.
all_possible_boards(CurrentBoard, [Move | Moves], [NewBoard | NewBoards]) :-
    do_move(Move, CurrentBoard, NewBoard),
    all_possible_boards(CurrentBoard, Moves, NewBoards), !.
all_possible_boards(_, [], []) :- !.


%! do_move(+Move, +CurrentState, -NewState)
%
% Update the state with a given move for a given piece.
do_move(Move, CurrentState, NewState) :-
    Move = move(DeletePieces, AppendPieces, DeleteRokades, NewPassant),
    state:pieces(CurrentState, CurrentPieces),
    state:rokades(CurrentState, CurrentRokades),
    state:nextcolor(CurrentState, NewColor),

    % Delete pieces
    util:delete_list(CurrentPieces, DeletePieces, NewPiecesDeleted),

    % Append pieces
    append(NewPiecesDeleted, AppendPieces, NewPieces),

    % Delete Rokades
    util:delete_list(CurrentRokades, DeleteRokades, NewRokades),

    % Create the new state
    NewState = state(NewPieces, NewColor, NewRokades, NewPassant).


%! all_possible_moves(+State, -Moves)
%
%  All possible moves for the current state.
all_possible_moves(State, Moves) :-
    state:pieces(State, Pieces),
    state:currentcolor(State, CurrentColor),

    % Get the pieces for the given color
    pieces:color_pieces(CurrentColor, Pieces, ColorPieces),

    % Get all possible moves for the pieces
    % TODO: merge this statement inside this predicate using maplist & append.
    all_possible_moves_for_pieces(ColorPieces, State, Moves).


%! all_possible_moves_for_pieces(+Pieces, +State, -Moves)
%
%  All possible moves for all given pieces in the current state.
all_possible_moves_for_pieces([Piece | Pieces], State, Moves) :-

    % All possible moves for the current piece
    possible_moves(Piece, State, PieceMoves),

    % Recursive call
    all_possible_moves_for_pieces(Pieces, State, RestMoves),

    % Merge the moves into the moves list
    append(PieceMoves, RestMoves, Moves), !.
all_possible_moves_for_pieces([], _, []) :- !.


%! possible_moves(+Piece, +State, -Moves)
%
%  All possible moves for a specific piece in the given state.

% King
possible_moves(Piece, State, Moves) :-
    Piece = piece(_, king, _),

    % King can move in a square
    square_moves(Piece, State, Moves), !.

% Queen
possible_moves(Piece, State, Moves) :-
    Piece = piece(_, queen, _),

    % Queen can move diagonally or in a cross
    cross_moves(Piece, State, CrossMoves),
    diagonal_moves(Piece, State, DiagonalMoves),

    % Merge possible moves
    append([CrossMoves, DiagonalMoves], Moves), !.

% Tower
possible_moves(Piece, State, Moves) :-
    Piece = piece(_, tower, _),

    % Tower can move in a cross
    cross_moves(Piece, State, Moves), !.

% Bishop
possible_moves(Piece, State, Moves) :-
    Piece = piece(_, bishop, _),

    % Bishop can move in diagonally.
    diagonal_moves(Piece, State, Moves), !.

% Horse
possible_moves(Piece, State, Moves) :-
   Piece = piece(_, horse, _),

   % Horse positions
   findall(Position, positions:horse_position(Piece, State, Position), Positions),

   % Convert positions into moves
   positions_to_moves(Piece, State, Positions, Moves), !.

% Pawn
possible_moves(Piece, State, Moves) :-
    pieces:type(Piece, pawn),

    % Possible moves
    pawn_forward_moves(Piece, State, ForwardMoves),
    pawn_diagonal_moves(Piece, State, DiagonalMoves),
    pawn_promotion_moves(Piece, PromitionMoves),
    pawn_passant_moves(Piece, State, PassantMoves),

    % Merge possible moves
    append([ForwardMoves, DiagonalMoves, PromitionMoves, PassantMoves], Moves), !.

%! pawn_moves(+Piece, +State, -Moves)
% 
%  Moves for the pawn going forward
pawn_forward_moves(Piece, State, [Move1, Move2]) :- % Pawn on start position (can move 2 steps forward)
    pieces:type(Piece, pawn),
    pieces:position(Piece, CurrentPosition),

    % Pawn must be on start position
    positions:pawn_start_position(CurrentPosition, Color),

    % First position must be valid & empty (otherwise the pawn is not able to move 2 steps forward)
    positions:forward_position(CurrentPosition, Color, NewPosition1),
    positions:valid_position(NewPosition1),
    positions:empty_position(NewPosition1, State),

    % Second position must be valid & empty
    positions:forward_position(NewPosition1, Color, NewPosition2),
    positions:valid_position(NewPosition2),
    positions:empty_position(NewPosition2, State),

    % Create the moves
    create_move(CurrentPosition, NewPosition1, State, Move1), % En-passant possibility
    create_move(CurrentPosition, NewPosition2, State, [], passant(Color, NewPosition1), Move2), !.

pawn_forward_moves(Piece, State, [Move1]) :-      % Pawn (can move max 1 step forward)
    pieces:type(Piece, pawn),
    pieces:position(Piece, CurrentPosition),
    pieces:position(Piece, Color),

    % First position must be valid & empty
    positions:forward_position(CurrentPosition, Color, NewPosition1),
    positions:valid_position(NewPosition1),
    positions:empty_position(NewPosition1, State),

    % Create the moves
    create_move(CurrentPosition, NewPosition1, State, Move1), !.

pawn_forward_moves(Piece, _, []) :-             % Pawn cannot move forward
    pieces:type(Piece, pawn),
    !.                          

%! pawn_diagonal_moves(+Piece, +State, -Moves)
%
%  Moves for the given pawn moving diagonally
pawn_diagonal_moves(Piece, State, Moves) :-
    
    % Moves for both diagonal parts
    pawn_diagonal_moves_part(Piece, State, -1, LeftMoves),
    pawn_diagonal_moves_part(Piece, State, 1, RightMoves),

    % Merge the 2 lists
    append([LeftMoves, RightMoves], Moves).


%! pawn_diagonal_moves_part(+Piece, +State, +XDifference, -Moves)
%
%  Moves for the given pawn moving diagonally either left or right.
%  XDifference = 1: right diagonal move
%  XDifference = -1: left diagonal move
pawn_diagonal_moves_part(Piece, State, XDifference, [Move]) :- % Left diagonal
    pieces:type(Piece, pawn),
    pieces:position(Piece, X/Y),
    pieces:color(Piece, Color),

    % New position
    XNew is X + XDifference,
    positions:forward_position(XNew/Y, Color, XNew/YNew),

    % New position must be valid
    positions:valid_position(XNew/YNew),

    % New position must be taken by an opponent piece
    positions:opponent_position(XNew/YNew, Color, State),

    % Create the move
    create_move(X/Y, XNew/YNew, State, Move), !.

pawn_diagonal_moves_part(Piece, _, _, []) :-
    pieces:type(Piece, pawn), !.

%! pawn_promotion_moves(+Piece, -Moves)
%
%  Move for the given pawn if reaching a point of promotion
pawn_promotion_moves(Piece, Moves) :-
    pieces:type(Piece, pawn),
    pieces:position(Piece, X/Y),
    pieces:color(Piece, Color),

    % Pawn must be on promotion position
    positions:pawn_promotion_position(X/Y, Color),

    % Possible moves
    Moves = [
        move([Piece], [piece(Color, queen, X/Y)], [], none),
        move([Piece], [piece(Color, horse, X/Y)], [], none),
        move([Piece], [piece(Color, tower, X/Y)], [], none),
        move([Piece], [piece(Color, bishop, X/Y)], [], none)
    ], !.

pawn_promotion_moves(Piece, []) :-     
    pieces:type(Piece, pawn), !.  


%! pawn_passant_moves(+Piece, +State, -Moves)
%
%  Move for the given pawn if an en-passant move is possible
pawn_passant_moves(Piece, State, Moves) :-
    pieces:type(Piece, pawn),
    pieces:color(Piece, PieceColor),
    state:passant(State, Passant),

    Passant = passant(PassantColor, _), % TODO: passant:color(Passant, PassantColor)
    
    % En-passant position must be for the opponent
    positions:opponent(PieceColor, PassantColor),
        
    % En-pasant for both directions
    pawn_passant_moves_part(Piece, State, -1, LeftMoves),
    pawn_passant_moves_part(Piece, State, 1, RightMoves),

    % Merge the 2 lists
    append([LeftMoves, RightMoves], Moves), !.

pawn_passant_moves(Piece, _, []) :-     
    pieces:type(Piece, pawn), !.  


%! pawn_passant_moves_part(+Piece, +Passant, +XDifference, -Moves)
%
%  Moves for the given pawn doing en-passant either left or right
%  XDifference = 1: right en-passant move
%  XDifference = -1: left en-passant move
%
%  TODO: List of moves to single move (because a list is useless here)
pawn_passant_moves_part(Piece, State, XDifference, [Move]) :-
    pieces:type(Piece, pawn),
    pieces:color(Piece, PieceColor),
    pieces:position(Piece, X/Y),
    state:passant(State, Passant),

    Passant = passant(PassantColor, XPassant/YPassant), % TODO: passant:color(Passant, PassantColor), passant:position(Passant, XPassant/YPassant)

    % Check if the passant possibility is next to piece.
    XPassant is X + XDifference, 
    positions:forward_position(XPassant/Y, PieceColor, XPassant/YPassant),

    % New position of the pawn after en-passant
    positions:forward_position(XPassant/Y, PieceColor, XNew/YNew),

    % Piece to remove by doing the en-passant move
    OpponentPiece = piece(PassantColor, pawn, XPassant/Y),

    % Create the move
    Move = move([Piece, OpponentPiece], [piece(PieceColor, pawn, XNew/YNew)], [], none), !.

pawn_passant_moves_part(Piece, _, _, []) :- 
    pieces:type(Piece, pawn), !.


%! square_moves(+Piece, +State, -Moves)
%
%  Moves in a square around a given piece
square_moves(Piece, State, Moves) :-

    % Square positions
    findall(Position, positions:square_position(Piece, State, Position), Positions),

    % Convert positions into moves
    positions_to_moves(Piece, State, Positions, Moves).


%! cross_moves(+Piece, +State, -Moves)
%
%  Moves in a cross starting from a given piece
cross_moves(Piece, State, Moves) :-

    path_moves(Piece, State, 1, 0, RightMoves),   % Right row part
    path_moves(Piece, State, -1, 0, LeftMoves),   % Left row part
    path_moves(Piece, State, 0, 1, TopMoves),     % Top column part
    path_moves(Piece, State, 0, -1, BottomMoves), % Bottom column part

    % Merge lists
    append([RightMoves, LeftMoves, TopMoves, BottomMoves], Moves).


%! diagonal_moves(+Piece, +State, -Moves)
%
%  Moves on the diagonals starting from a given piece
diagonal_moves(Piece, State, Moves) :-
    path_moves(Piece, State, 1, 1, TopRightMoves),      % Top-right diagonal
    path_moves(Piece, State, -1, 1, TopLeftMoves),      % Top-right diagonal
    path_moves(Piece, State, 1, -1, BottomRightMoves),  % Bottom-right diagonal
    path_moves(Piece, State, -1, -1, BottomLeftMoves),  % Bottom-left diagonal

    % Merge lists
    append([TopRightMoves, TopLeftMoves, BottomRightMoves, BottomLeftMoves], Moves).


%! path_moves(+Piece, +State, +XDirection, +YDirection, -Moves)
%
%  Moves on a given path starting from a piece and with incremental addition of (XDirection, YDirection)
%  Will stop the path when a new position is either invalid or blocked by another piece
%
%  TODO: ask prof about code duplication
path_moves(Piece, State, XDirection, YDirection, [Move | Moves]) :-
    pieces:color(Piece, Color),
    pieces:type(Piece, Type),
    pieces:position(Piece, X/Y),
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Unify the new piece
    NewPiece = piece(Color, Type, XNew/YNew),

    % Create the move
    create_move(X/Y, XNew/YNew, State, Move),

    % New position must be valid
    positions:valid_position(XNew/YNew),

    % New position must be empty
    positions:empty_position(XNew/YNew, State), !,

    % Recursivly extend the diagonal
    path_moves(NewPiece, State, XDirection, YDirection, Moves).

path_moves(Piece, State, XDirection, YDirection, [Move]) :-
    pieces:color(Piece, Color),
    pieces:position(Piece, X/Y),
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Create the move
    create_move(X/Y, XNew/YNew, State, Move),

    % New position must be valid
    positions:valid_position(XNew/YNew),

    % New position must be taken by the opponent
    positions:opponent_position(XNew/YNew, Color, State), !.

path_moves(_, _, _, _, []) :- !.

%! positions_to_moves(+Piece, +Stat, +Positions, -Moves)
%
%  Corresponding moves for a given set of positions
positions_to_moves(Piece, State, [NextPosition | NextPositions], [Move | Moves]) :-
    pieces:position(Piece, CurrentPosition),

    % Construct the move
    create_move(CurrentPosition, NextPosition, State, Move),
    
    % Recursive Call
    positions_to_moves(Piece, State, NextPositions, Moves), !.
positions_to_moves(_, _, [], []) :- !.


%! create_move/4(+CurrentPosition, +NewPosition, +State, -Move)
%
%  Create a move from a given position to a new position.
create_move(CurrentPosition, NewPosition, State, Move) :-
    create_move(CurrentPosition, NewPosition, State, [], none, Move).


%! create_move/6(+CurrentPosition, +NewPosition, +State, +DeleteRokades, +Passant, -Move)
%
%  Create a move for a given piece, position, rokades to delete and en-passant possability
create_move(CurrentPosition, NewPosition, State, DeleteRokades, Passant, Move) :- % Opponent on new position
    state:piece(CurrentPosition, State, piece(Color, Type, _)),

    % Opponent at the new position
    positions:opponent_position(NewPosition, Color, State, OpponentPiece),

    % Create the pieces
    CurrentPiece = piece(Color, Type, CurrentPosition),
    NewPiece     = piece(Color, Type, NewPosition),

    % Unify the move
    Move = move([CurrentPiece, OpponentPiece], [NewPiece], DeleteRokades, Passant), !.

create_move(CurrentPosition, NewPosition, State, DeleteRokades, Passant, Move) :-  % No piece on new position
    state:piece(CurrentPosition, State, piece(Color, Type, _)),

    % Empty new position
    positions:empty_position(NewPosition, State),

    % Create the pieces
    CurrentPiece = piece(Color, Type, CurrentPosition),
    NewPiece     = piece(Color, Type, NewPosition),

    % Unify the move
    Move = move([CurrentPiece], [NewPiece], DeleteRokades, Passant), !.