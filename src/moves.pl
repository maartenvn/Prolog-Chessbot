:- module(moves, []).

:- use_module("state").
:- use_module("positions").
:- use_module("pieces").
:- use_module("util").

%! all_possible_states/2(+CurrentState, -NextStates)
%
%  Generate all possible next states for a given state
all_possible_states(CurrentState, NextStates) :-
    state:currentcolor(CurrentState, CurrentColor),

    % All pseudo-possible moves for the next player
    moves:all_possible_moves(CurrentColor, CurrentState, NextMoves),

    % Helper predicate
    all_possible_states(CurrentState, NextMoves, NextStates).


%! all_possible_states/3(+CurrentState, +Moves, -NextStates)
%
%  Generate a new state for every possible move and append it to a list.
all_possible_states(CurrentState, [Move | Moves], [NextState | NextStates]) :-  % Valid pseudo-move
    state:currentcolor(CurrentState, CurrentColor),

    % Do the move and retrieve the new state
    do_move(Move, CurrentState, NextState),

    % State must not be in-check
    % If this state causes a check, it is not a valid state
    not(state:check(NextState, CurrentColor)),
    
    % Recursive call
    all_possible_states(CurrentState, Moves, NextStates), !.
all_possible_states(CurrentState, [_ | Moves], NextStates) :-                   % Invalid pseudo-move
    % Recursive call
    all_possible_states(CurrentState, Moves, NextStates), !.
all_possible_states(_, [], []) :- !.                                            % Base-Case


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
    state:color_pieces(Color, State, ColorPieces),

    % Get all possible moves for the pieces
    % TODO: merge this statement inside this predicate using maplist & append.
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
all_possible_moves_for_pieces([], _, []) :- !.


%! possible_moves(+Piece, +State, -Moves)
%
%  All psuedo-possible moves for a specific piece in the given state.
%  This predicate will also include moves that cause a potential in-check situation.

% King
possible_moves(Piece, State, Moves) :-
    pieces:type(Piece, king),

    % King can move in a square
    square_moves(Piece, State, Moves), !.

% Queen
possible_moves(Piece, State, Moves) :-
    pieces:type(Piece, queen),

    % Queen can move diagonally or in a cross
    cross_moves(Piece, State, CrossMoves),
    diagonal_moves(Piece, State, DiagonalMoves),

    % Merge possible moves
    append([CrossMoves, DiagonalMoves], Moves), !.

% Tower
possible_moves(Piece, State, Moves) :-
    pieces:type(Piece, tower),

    % Tower can move in a cross
    cross_moves(Piece, State, Moves), !.

% Bishop
possible_moves(Piece, State, Moves) :-
    pieces:type(Piece, bishop),

    % Bishop can move in diagonally.
    diagonal_moves(Piece, State, Moves), !.

% Horse
possible_moves(Piece, State, Moves) :-
    pieces:type(Piece, horse),

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
    pieces:color(Piece, Color),
    Move = move(DeletePieces, AppendPieces, _, _),

    % Select the pawn
    select(piece(Color, pawn, NewPosition), AppendPieces, _),

    % Check if the pawn position is a promotion position
    positions:pawn_promotion_position(NewPosition, Color),

    % Recursive call
    convert_promotion_moves(Piece, Moves, PromotionMovesRest),

    % Create the promotion moves
    PromotionMovesCurrent = [
        move(DeletePieces, [piece(Color, queen, NewPosition)], [], none),
        move(DeletePieces, [piece(Color, horse, NewPosition)], [], none),
        move(DeletePieces, [piece(Color, tower, NewPosition)], [], none),
        move(DeletePieces, [piece(Color, bishop, NewPosition)], [], none)
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
    pieces:type(Piece, pawn),
    pieces:color(Piece, Color),
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
    create_move(CurrentPosition, NewPosition2, State, passant(Color, NewPosition1), Move2), !.

pawn_forward_moves(Piece, State, [Move1]) :-       % Pawn (can move max 1 step forward)
    pieces:type(Piece, pawn),
    pieces:position(Piece, CurrentPosition),
    pieces:color(Piece, Color),

    % Forward position must be valid & empty
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

%! path_moves/5(+Piece, +State, +XDirection, +YDirection, -Moves)
%
%  Moves on a given path starting from a piece and with incremental addition of (XDirection, YDirection)
%  Will stop the path when a new position is either invalid or blocked by another piece
path_moves(Piece, State, XDirection, YDirection, Moves) :-
    pieces:position(Piece, X/Y),

    path_moves(Piece, X/Y, State, XDirection, YDirection, Moves).


%! path_moves/6(+StartPiece, +PreviousPosition, +State, +XDirection, +YDirection, -Moves)
%
%  Helper function for path_moves/5.
%  Uses a StartPiece to correctly form the moves.  
%
%  TODO: ask prof about code duplication
path_moves(StartPiece, X/Y, State, XDirection, YDirection, [Move | Moves]) :-
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Create the move
    create_piece_move(StartPiece, XNew/YNew, State, Move),

    % New position must be valid
    positions:valid_position(XNew/YNew),

    % New position must be empty
    positions:empty_position(XNew/YNew, State), !,

    % Recursivly extend the diagonal
    path_moves(StartPiece, XNew/YNew, State, XDirection, YDirection, Moves).

path_moves(StartPiece, X/Y, State, XDirection, YDirection, [Move]) :-
    pieces:color(StartPiece, Color),
    
    % Unify the new position
    XNew is X + XDirection,
    YNew is Y + YDirection,

    % Create the move
    create_piece_move(StartPiece, XNew/YNew, State, Move),

    % New position must be valid
    positions:valid_position(XNew/YNew),

    % New position must be taken by the opponent
    positions:opponent_position(XNew/YNew, Color, State), !.

path_moves(_, _, _, _, _, []) :- !.

%! positions_to_moves(+Piece, +Stat, +Positions, -Moves)
%
%  Corresponding moves for a given set of positions
positions_to_moves(Piece, State, [NextPosition | NextPositions], [Move | Moves]) :-

    % Construct the move
    create_piece_move(Piece, NextPosition, State, Move),
    
    % Recursive Call
    positions_to_moves(Piece, State, NextPositions, Moves), !.
positions_to_moves(_, _, [], []) :- !.


%! create_move/4(+CurrentPosition, +NewPosition, +State, -Move)
%
%  Create a move from a given position to a new position.
create_move(CurrentPosition, NewPosition, State, Move) :-
    create_move(CurrentPosition, NewPosition, State, none, Move).


%! create_move/6(+CurrentPosition, +NewPosition, +State, +Passant, -Move)
%
%  Create a move for given positions and en-passant possability
create_move(CurrentPosition, NewPosition, State, Passant, Move) :-
    state:position_piece(CurrentPosition, State, CurrentPiece),
    create_piece_move(CurrentPiece, NewPosition, State, Passant, Move).


%! create_move/4(+CurrentPiece, +NewPosition, +State, -Move)
%
%  Create a move from a given position to a new position.
create_piece_move(CurrentPiece, NewPosition, State, Move) :-
    create_piece_move(CurrentPiece, NewPosition, State, none, Move).


%! create_move/6(+CurrentPiece, +NewPosition, +State, +Passant, -Move)
%
%  Create a move for a given piece, position and en-passant possability
create_piece_move(CurrentPiece, NewPosition, State, Passant, Move) :- % Opponent on new position
    pieces:color(CurrentPiece, Color),
    pieces:type(CurrentPiece, Type),
    pieces:position(CurrentPiece, Position),

    % Opponent color
    positions:opponent(Color, OpponentColor),

    % Opponent at the new position
    positions:opponent_position(NewPosition, Color, State, OpponentPiece),

    % Create the new piece
    NewPiece = piece(Color, Type, NewPosition),

    % Rokades to delete
    positions:rokades_position(Position, Color, DeleteRokadesFromMove),        % If king/tower move
    positions:rokades_position(NewPosition, OpponentColor, DeleteRokadesFromCapture),  % If king/tower is captured
    append([DeleteRokadesFromMove, DeleteRokadesFromCapture], DeleteRokades),

    % Unify the move
    Move = move([CurrentPiece, OpponentPiece], [NewPiece], DeleteRokades, Passant), !.

create_piece_move(CurrentPiece, NewPosition, State, Passant, Move) :-  % No piece on new position
    pieces:color(CurrentPiece, Color),
    pieces:type(CurrentPiece, Type),
    pieces:position(CurrentPiece, Position),

    % Empty new position
    positions:empty_position(NewPosition, State),

    % Create the new piece
    NewPiece = piece(Color, Type, NewPosition),

    % Rokades to delete
    positions:rokades_position(Position, Color, DeleteRokades),

    % Unify the move
    Move = move([CurrentPiece], [NewPiece], DeleteRokades, Passant), !.


