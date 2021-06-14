:- module(move, []).

:- use_module("state").
:- use_module("position").
:- use_module("piece").


%! delete_pieces(+Move, -DeletePieces)
%
%  List of pieces to delete from the board for a given move
delete_pieces(move(DeletePieces, _), DeletePieces).


%! append_pieces(+Move, -AppendPieces)
%
%  List of pieces to append to the board for a given move
append_pieces(move(_, AppendPieces), AppendPieces).


%! delete_rokades(+Move, -DeleteRokades)
%
%  List of rokades to delete for a given move
delete_rokades(Move, DeleteRokades) :-
    move:delete_pieces(Move, DeletePieces),

    % Convert a list of pieces into a list of rokades
    maplist([Piece, Rokades] >> (piece:rokades_piece(Piece, Rokades)), DeletePieces, PossibleRokades),

    % Merge all possible rokades
    append(PossibleRokades, DeleteRokades).


%! new_passant(+Move, -NewPassant)
%
%  Get the new en-passant possibility for a given move
new_passant(Move, NewPassant) :-
    % When moving a piece 2 steps forward (and creating an en-passant possibility)
    % the piece will never attack an opponent
    move:delete_pieces(Move, [piece(Color, pawn, OldPosition)]),
    move:append_pieces(Move, [piece(Color, pawn, NewPosition)]),

    % Find the en-passant possibility
    position:pawn_start_position(OldPosition, Color),
    piece:passant_piece(piece(Color, pawn, NewPosition), NewPassant), !.
new_passant(_, none).


%! do_move(+Move, +CurrentState, -NewState)
%
% Update the state with a given move for a given piece.
do_move(Move, CurrentState, NewState) :-
    move:delete_pieces(Move, DeletePieces),
    move:append_pieces(Move, AppendPieces),
    move:delete_rokades(Move, DeleteRokades),
    move:new_passant(Move, NewPassant),

    % Next color
    state:nextcolor(CurrentState, NewColor),

    % Delete pieces
    state:remove_pieces(CurrentState, DeletePieces, PartialState1),

    % Add pieces
    state:set_pieces(PartialState1, AppendPieces, PartialState2),

    % Delete Rokades
    state:remove_rokades(PartialState2, DeleteRokades, PartialState3),

    % Update passant
    state:set_passant(PartialState3, NewPassant, PartialState4),

    % Update color
    state:set_color(PartialState4, NewColor, NewState).


%! all_possible_moves/2(+State, -Moves)
%
%  All pseudo-possible moves for the current state.
all_possible_moves(State, Moves) :-
    state:currentcolor(State, CurrentColor),
    all_possible_moves(CurrentColor, State, Moves).


%! all_possible_moves/3(+Color, +State, -Moves)
%
%  All pseudo-possible moves for the current state for a given color.
all_possible_moves(Color, State, Moves) :-
    
    % Get the pieces for the given color
    state:color_pieces(State, Color, ColorPieces),

    % Get all possible moves for the pieces
    all_possible_moves_for_pieces(ColorPieces, State, Moves).


%! all_possible_moves_for_pieces(+Pieces, +State, -Moves)
%
%  All pseudo-possible moves for all given pieces in the current state.
all_possible_moves_for_pieces([Piece | Pieces], State, Moves) :-

    % All possible moves for the current piece
    possible_moves(Piece, State, PieceMoves),

    % Recursive call
    all_possible_moves_for_pieces(Pieces, State, RestMoves),

    % Merge the moves into the moves list
    append(PieceMoves, RestMoves, Moves), !.
all_possible_moves_for_pieces([], _, []).

%! valid_move(+State, +Move)
%
%  If a given move is valid.
%  A move is considered invalid if it causes a check.
valid_move(State, Move) :-
    state:currentcolor(State, CurrentColor),

    % Do the move and retrieve the new state
    move:do_move(Move, State, NextState),

    % State is check
    not(state:check(NextState, CurrentColor)).


%! valid_moves(+State, +Moves, ?ValidMoves)
%
%  Filter a given list of moves by removing all moves that lead to a check of the current color.
valid_moves(_, [], []).
valid_moves(State, [Move | Moves], ValidMoves) :-  % Move is valid

    % Check if move is valid
    % This cut operator is to prevent having to use "not(valid_move(...))" in the
    % next predicate. This is for performance reasons.
    valid_move(State, Move), !,

    % Assign the valid move
    ValidMoves = [Move | ValidMovesRest],

    % Recursive call
    valid_moves(State, Moves, ValidMovesRest).
valid_moves(State, [_ | Moves], ValidMoves) :-              % Move is invalid
    % Recursive call
    valid_moves(State, Moves, ValidMoves).


%! possible_moves(+Piece, +State, -Moves)
%
%  All psuedo-possible moves for a specific piece in the given state.
%  This predicate will also include moves that cause a potential in-check situation.

% King
possible_moves(Piece, State, Moves) :-
    piece:type(Piece, king),

    % Rokades
    findall(Move, rokades_move(Piece, State, Move), RokadesMoves),

    % King can move in a square
    square_moves(Piece, State, SquareMoves),
    
    % Merge possible moves
    append([RokadesMoves, SquareMoves], Moves), !.

% Queen
possible_moves(Piece, State, Moves) :-
    piece:type(Piece, queen),

    % Queen can move diagonally or in a cross
    cross_moves(Piece, State, CrossMoves),
    diagonal_moves(Piece, State, DiagonalMoves),

    % Merge possible moves
    append([CrossMoves, DiagonalMoves], Moves), !.

% Tower
possible_moves(Piece, State, Moves) :-
    piece:type(Piece, tower),

    % Tower can move in a cross
    cross_moves(Piece, State, Moves), !.

% Bishop
possible_moves(Piece, State, Moves) :-
    piece:type(Piece, bishop),

    % Bishop can move in diagonally.
    diagonal_moves(Piece, State, Moves), !.

% Horse
possible_moves(Piece, State, Moves) :-
    piece:type(Piece, horse),

   % Horse positions
   findall(Position, position:horse_position(Piece, State, Position), Positions),

   % Convert positions into moves
   positions_to_moves(Piece, State, Positions, Moves), !.

% Pawn
possible_moves(Piece, State, Moves) :-
    piece:type(Piece, pawn),

    % Possible moves
    pawn_forward_moves(Piece, State, ForwardMoves),
    pawn_diagonal_moves(Piece, State, DiagonalMoves),
    pawn_passant_moves(Piece, State, PassantMoves),

    % Merge possible moves
    append([ForwardMoves, DiagonalMoves, PassantMoves], MergedMoves),
    
    % Handle potential pawn promotional moves
    convert_promotion_moves(Piece, MergedMoves, Moves), !.


%! convert_promotion_moves(+Piece, +Moves, +PromotionMoves)
%
%  Convert a list of moves to a list of promotion moves.
%  Will scan every move, check if the piece can be promoted, and create the correct promotions
convert_promotion_moves(Piece, [Move | Moves], PromotionMoves) :-    % Current move is promotion move
    piece:color(Piece, Color),
    Move = move(DeletePieces, AppendPieces),

    % Select the pawn
    select(piece(Color, pawn, NewPosition), AppendPieces, _),

    % Check if the pawn position is a promotion position
    position:pawn_promotion_position(NewPosition, Color),

    % Recursive call
    convert_promotion_moves(Piece, Moves, PromotionMovesRest),

    % Create the promotion moves
    PromotionMovesCurrent = [
        move(DeletePieces, [piece(Color, queen, NewPosition)]),
        move(DeletePieces, [piece(Color, horse, NewPosition)]),
        move(DeletePieces, [piece(Color, tower, NewPosition)]),
        move(DeletePieces, [piece(Color, bishop, NewPosition)])
    ],

    % Merge
    append([PromotionMovesCurrent, PromotionMovesRest], PromotionMoves), !.

convert_promotion_moves(Piece, [Move | Moves], [PromotionMove | PromotionMoves]) :-    % Current move is not a promotion move

    % Add the old move to the promotion moves
    % since the original moves, that are no promotions, must be included as well
    PromotionMove = Move,
    
    % Recursive call
    convert_promotion_moves(Piece, Moves, PromotionMoves), !.

convert_promotion_moves(_, [], []) :- !. % Base Case


%! pawn_moves(+Piece, +State, -Moves)
% 
%  Moves for the pawn going forward
pawn_forward_moves(Piece, State, [Move1, Move2]) :- % Pawn on start position (can move 2 steps forward)
    piece:type(Piece, pawn),
    piece:color(Piece, Color),
    piece:position(Piece, CurrentPosition),

    % Pawn must be on start position
    position:pawn_start_position(CurrentPosition, Color),

    % First position must be valid & empty (otherwise the pawn is not able to move 2 steps forward)
    position:forward_position(CurrentPosition, Color, NewPosition1),
    position:valid_position(NewPosition1),
    position:empty_position(NewPosition1, State),

    % Second position must be valid & empty
    position:forward_position(NewPosition1, Color, NewPosition2),
    position:valid_position(NewPosition2),
    position:empty_position(NewPosition2, State),

    % Create the moves
    create_move(CurrentPosition, NewPosition1, State, Move1),
    create_move(CurrentPosition, NewPosition2, State, Move2), !.

pawn_forward_moves(Piece, State, [Move1]) :-        % Pawn (can move max 1 step forward)
    piece:type(Piece, pawn),
    piece:position(Piece, CurrentPosition),
    piece:color(Piece, Color),

    % Forward position must be valid & empty
    position:forward_position(CurrentPosition, Color, NewPosition1),
    position:valid_position(NewPosition1),
    position:empty_position(NewPosition1, State),

    % Create the moves
    create_move(CurrentPosition, NewPosition1, State, Move1), !.

pawn_forward_moves(Piece, _, []) :-             % Pawn cannot move forward
    piece:type(Piece, pawn),
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
    piece:type(Piece, pawn),
    piece:position(Piece, X/Y),
    piece:color(Piece, Color),

    % New position
    XNew is X + XDifference,
    position:forward_position(XNew/Y, Color, XNew/YNew),

    % New position must be valid
    position:valid_position(XNew/YNew),

    % New position must be taken by an opponent piece
    position:opponent_position(XNew/YNew, Color, State),

    % Create the move
    create_move(X/Y, XNew/YNew, State, Move), !.

pawn_diagonal_moves_part(Piece, _, _, []) :-
    piece:type(Piece, pawn), !.  


%! pawn_passant_moves(+Piece, +State, -Moves)
%
%  Move for the given pawn if an en-passant move is possible
pawn_passant_moves(Piece, State, Moves) :-
    piece:type(Piece, pawn),
    piece:color(Piece, PieceColor),
    state:passant(State, Passant),

    Passant = passant(PassantColor, _),
    
    % En-passant position must be for the opponent
    piece:opponent(PieceColor, PassantColor),
        
    % En-pasant for both directions
    pawn_passant_moves_part(Piece, State, -1, LeftMoves),
    pawn_passant_moves_part(Piece, State, 1, RightMoves),

    % Merge the 2 lists
    append([LeftMoves, RightMoves], Moves), !.

pawn_passant_moves(Piece, _, []) :-     
    piece:type(Piece, pawn), !.  


%! pawn_passant_moves_part(+Piece, +Passant, +XDifference, -Moves)
%
%  Moves for the given pawn doing en-passant either left or right
%  XDifference = 1: right en-passant move
%  XDifference = -1: left en-passant move
pawn_passant_moves_part(Piece, State, XDifference, [Move]) :-
    piece:type(Piece, pawn),
    piece:color(Piece, PieceColor),
    piece:position(Piece, X/Y),
    state:passant(State, Passant),

    Passant = passant(PassantColor, XPassant/YPassant),

    % Check if the passant possibility is next to piece.
    XPassant is X + XDifference, 
    position:forward_position(XPassant/Y, PieceColor, XPassant/YPassant),

    % New position of the pawn after en-passant
    position:forward_position(XPassant/Y, PieceColor, XNew/YNew),

    % Piece to remove by doing the en-passant move
    OpponentPiece = piece(PassantColor, pawn, XPassant/Y),

    % Create the move
    Move = move([Piece, OpponentPiece], [piece(PieceColor, pawn, XNew/YNew)]), !.

pawn_passant_moves_part(Piece, _, _, []) :- 
    piece:type(Piece, pawn), !.


%! rokades_move(+King, +State, -Moves)
%
%  Rokades move for the king
rokades_move(King, State, Move) :-   % Short rokade 
    piece:color(King, Color),
    piece:position(King, KingPosition),
    state:rokades(State, Rokades),
    _/Y = KingPosition,

    % Short rokade
    ShortRokade = rokade(Color, short),
    memberchk(ShortRokade, Rokades),

    % Tower for rokade
    Tower = piece(Color, tower, TowerPosition),
    piece:rokades_piece(Tower, [ShortRokade]),

    % Check if the pieces between the tower and king are empty
    position:empty_between_positions(KingPosition, TowerPosition, State),

    % New pieces
    NewKing  = piece(Color, king,  7/Y),
    NewTower = piece(Color, tower, 6/Y),

    % Create the move
    Move = move([King, Tower], [NewKing, NewTower]).

rokades_move(King, State, Move) :-   % Long rokade 
    piece:color(King, Color),
    piece:position(King, KingPosition),
    state:rokades(State, Rokades),
    _/Y = KingPosition,

    % Long rokade
    LongRokade = rokade(Color, long),
    memberchk(LongRokade, Rokades),

    % Tower for rokade
    Tower = piece(Color, tower, TowerPosition),
    piece:rokades_piece(Tower, [LongRokade]),

    % Check if the pieces between the tower and king are empty
    position:empty_between_positions(TowerPosition, KingPosition, State),

    % New pieces
    NewKing  = piece(Color, king,  3/Y),
    NewTower = piece(Color, tower, 4/Y),

    % Create the move
    Move = move([King, Tower], [NewKing, NewTower]).


%! square_moves(+Piece, +State, -Moves)
%
%  Moves in a square around a given piece
square_moves(Piece, State, Moves) :-

    % Square positions
    findall(Position, position:square_position(Piece, State, Position), Positions),

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

%! path_moves/5(+Piece, +State, +XDirection, +YDirection, -Moves)
%
%  Moves on a given path starting from a piece and with incremental addition of (XDirection, YDirection)
%  Will stop the path when a new position is either invalid or blocked by another piece
path_moves(Piece, State, XDirection, YDirection, Moves) :-
    piece:position(Piece, X/Y),

    path_moves(Piece, X/Y, State, XDirection, YDirection, Moves).


%! path_moves/6(+StartPiece, +PreviousPosition, +State, +XDirection, +YDirection, -Moves)
%
%  Helper function for path_moves/5.
%  Uses a StartPiece to correctly form the moves.  
path_moves(StartPiece, X/Y, State, XDirection, YDirection, [Move | Moves]) :-
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Create the move
    create_piece_move(StartPiece, XNew/YNew, State, Move),

    % New position must be valid
    position:valid_position(XNew/YNew),

    % New position must be empty
    position:empty_position(XNew/YNew, State), !,

    % Recursivly extend the diagonal
    path_moves(StartPiece, XNew/YNew, State, XDirection, YDirection, Moves).
path_moves(StartPiece, X/Y, State, XDirection, YDirection, [Move]) :-
    piece:color(StartPiece, Color),
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Create the move
    create_piece_move(StartPiece, XNew/YNew, State, Move),

    % New position must be valid
    position:valid_position(XNew/YNew),

    % New position must be taken by the opponent
    position:opponent_position(XNew/YNew, Color, State), !.
path_moves(_, _, _, _, _, []).

%! positions_to_moves(+Piece, +Stat, +Positions, -Moves)
%
%  Corresponding moves for a given set of positions
positions_to_moves(Piece, State, [NextPosition | NextPositions], [Move | Moves]) :-

    % Construct the move
    create_piece_move(Piece, NextPosition, State, Move),
    
    % Recursive Call
    positions_to_moves(Piece, State, NextPositions, Moves), !.
positions_to_moves(_, _, [], []).


%! create_move/4(+CurrentPosition, +NewPosition, +State, -Move)
%
%  Create a move from a given position to a new position.
create_move(CurrentPosition, NewPosition, State, Move) :-
    state:piece_at_position(State, CurrentPosition, CurrentPiece),
    create_piece_move(CurrentPiece, NewPosition, State, Move).


%! create_move/4(+CurrentPiece, +NewPosition, +State, -Move)
%
%  Create a move for a given piece, position and en-passant possability
create_piece_move(CurrentPiece, NewPosition, State, Move) :-  % Opponent on new position
    piece:color(CurrentPiece, Color),
    piece:type(CurrentPiece, Type),

    % Opponent at the new position
    position:opponent_position(NewPosition, Color, State, OpponentPiece),

    % Create the new piece
    NewPiece = piece(Color, Type, NewPosition),

    % Unify the move
    Move = move([CurrentPiece, OpponentPiece], [NewPiece]), !.
create_piece_move(CurrentPiece, NewPosition, State, Move) :-  % No piece on new position
    piece:color(CurrentPiece, Color),
    piece:type(CurrentPiece, Type),

    % Empty new position
    position:empty_position(NewPosition, State),

    % Create the new piece
    NewPiece = piece(Color, Type, NewPosition),

    % Unify the move
    Move = move([CurrentPiece], [NewPiece]), !.


