:- module(state, []).

:- use_module("move").
:- use_module("piece").
:- use_module("position").


%! board(+State, -Board)
%
%  Extract the board from the given state.
board(state(Board, _, _, _), Board).


%! currentcolor(+State, -CurrentColor)
%
%  Extract the current color (the player that can do the current move) from the given state.
currentcolor(state(_, CurrentColor, _, _), CurrentColor).


%! nextcolor(+State, -NextColor)
%
%  Extract the next color (the player that can do a move after the current player did a move) from the given state.
nextcolor(state(_, white, _, _), black).
nextcolor(state(_, black, _, _), white).


%! rokades(+State, -Rokades)
%
%  Extract the remaining rokades from the given state.
rokades(state(_, _, Rokades, _), Rokades).


%! passant(+State, -Passant)
%
%  Extract the en-passant possibility from the given state.
passant(state(_, _, _, Passant), Passant).


%! empty_state(+CurrentColor, +Rokades, +Passant, -State)
%
%  Create a state with an empty board.
empty_state(CurrentColor, Rokades, Passant, State) :-
    
    % Create an empty board
    Board = rows(
        row(none, none, none, none, none, none, none, none),
        row(none, none, none, none, none, none, none, none),
        row(none, none, none, none, none, none, none, none),
        row(none, none, none, none, none, none, none, none),
        row(none, none, none, none, none, none, none, none),
        row(none, none, none, none, none, none, none, none),
        row(none, none, none, none, none, none, none, none),
        row(none, none, none, none, none, none, none, none)
    ),

    % Create the state
    State = state(Board, CurrentColor, Rokades, Passant).


%! create_state(+Pieces, +CurrentColor, +Rokades, +Passant, -State)
%
%  Create a state from a list of pieces
create_state(Pieces, CurrentColor, Rokades, Passant, State) :-
    
    % Create an empty state
    empty_state(CurrentColor, Rokades, Passant, EmptyState),

    % Set the pieces
    set_pieces(EmptyState, Pieces, State).


%! all_possible_states/2(+CurrentState, -NextStates)
%
%  Generate all possible next states for a given state
all_possible_states(CurrentState, NextStates) :-
    state:currentcolor(CurrentState, CurrentColor),

    % All pseudo-possible moves for the next player
    move:all_possible_moves(CurrentColor, CurrentState, NextMoves),

    % Helper predicate
    all_possible_states(CurrentState, NextMoves, NextStates).


%! all_possible_states/3(+CurrentState, +Moves, ?NextStates)
%
%  Generate a new state for every possible move and append it to a list.
all_possible_states(CurrentState, [Move | Moves], NextStates) :-  % Valid pseudo-move
    state:currentcolor(CurrentState, CurrentColor),

    % Do the move and retrieve the new state
    move:do_move(Move, CurrentState, NextState),

    (
        % State is check and should not be included
        state:check(NextState, CurrentColor),
        OtherStates = NextStates
        ;
        % State is not check and should be included
        [NextState | OtherStates] = NextStates
    ),
    
    % Recursive call
    all_possible_states(CurrentState, Moves, OtherStates), !.
all_possible_states(_, [], []).                                  % Base case


%! pieces/2(+State, -Pieces)
%
%  List of pieces that are currently on the board
pieces(State, Pieces) :-

    % Find all possible positions on the board
    position:valid_positions(Positions),
    
    % Helper predicate
    pieces(State, Positions, Pieces).


%! pieces/3(+State, +Positions, -Pieces)
%
%  Helper predicate for pieces/3
pieces(State, [Position | Positions], [Piece | Pieces]) :- % Piece at current position is not "none"
    
    % Piece at the current position
    piece_at_position(State, Position, Piece),

    % Piece must not be none
    Piece \== none,

    % Recursive call
    pieces(State, Positions, Pieces), !.
pieces(State, [_ | Positions], Pieces) :-                  % Piece at current position is "none"

    % Recursive call
    pieces(State, Positions, Pieces), !.
pieces(_, [], []).                                         % Base case


%! color_pieces/3(+State, +Color, -ColorPieces)
%
%  Unify all pieces for a given Color from the given state.
color_pieces(State, Color, ColorPieces) :-
    % Find all possible positions on the board
    position:valid_positions(Positions),

    % Helper predicate
    color_pieces(State, Color, Positions, ColorPieces).


%! color_pieces/4(+State, +Color, +Positions, -ColorPieces)
%
%  Helper predicate for color_pieces/3
color_pieces(State, Color, [Position | Positions], [ColorPiece | ColorPieces]) :- % Piece at current position is of the given color
    
    % Piece at the current position
    piece_at_position(State, Position, ColorPiece),

    % Color must match
    piece:color(ColorPiece, Color),

    % Recursive call
    color_pieces(State, Color, Positions, ColorPieces), !.
color_pieces(State, Color, [_ | Positions], ColorPieces) :-                      % Piece at current position is "none" or not of the given color
    
    % Recursive call
    color_pieces(State, Color, Positions, ColorPieces), !.
color_pieces(_, _, [], []).                                                      % Base case

%! king/3(+State, +Color, -King).
% 
%  Get the king piece for a given color
king(State, Color, King) :-
    % Find all possible positions on the board
    position:valid_positions(Positions),

    % Helper predicate
    king(Positions, State, Color, King).

%! king/4(+Positions, +State, +Color, -King).
% 
%  Helper predicate for king/3
king([Position | _], State, Color, King) :-
    % King Piece
    King = piece(Color, king, Position),

    % Piece at the current position is the king
    piece_at_position(State, Position, King).
king([_ | Positions], State, Color, King) :-
    % Recursive call
    king(Positions, State, Color, King).


%! piece_at_position(+State, +X/+Y, -Piece)
%
%  Piece at a given position in a given state.
piece_at_position(State, X/Y, Piece) :-
    state:board(State, Board),

    % Requested row
    arg(Y, Board, Row),

    % Requested piece
    arg(X, Row, Piece).


%! set_piece_at_position(+State, +Piece, +X/+Y, -NewState)
%
%  Set a piece at a given position on the board of the given state.
set_piece_at_position(State, Piece, X/Y, NewState) :-
    state:board(State, Board),

    % Get the row of the given position
    arg(Y, Board, Row),

    % Duplicate board & row to prevent setarg from altering other states.
    duplicate_term(Board, NewBoard),
    duplicate_term(Row, NewRow),

    % Update the position in the row
    setarg(X, NewRow, Piece),

    % Update the board
    setarg(Y, NewBoard, NewRow),
    
    % Update the state
    set_board(State, NewBoard, NewState), !.


%! set_piece(+State, +Piece, -NewState)
%
%  Set a piece on the board in a given state.
set_piece(State, Piece, NewState) :-
    piece:position(Piece, X/Y),

    % Set the piece at the given position
    set_piece_at_position(State, Piece, X/Y, NewState).


%! set_pieces(+State, +Pieces, +NewState)
%
%  Set a list of pieces on the board in a given state.
set_pieces(State, [], State).
set_pieces(State, [Piece | Pieces], NewState) :-
   
    % Set the piece
    set_piece(State, Piece, PartialState),
    
    % Recursive call
    set_pieces(PartialState, Pieces, NewState), !.


%! remove_piece(+State, +Piece, -NewState)
%
%  Remove a piece from the board in a given state.
remove_piece(State, Piece, NewState) :-
    piece:position(Piece, X/Y),

    % Set the piece position to none
    set_piece_at_position(State, none, X/Y, NewState).


%! remove_pieces(+State, +Pieces, -NewState)
%
%  Remove a list of pieces from the board
remove_pieces(State, [], State).
remove_pieces(State, [Piece | Pieces], NewState) :-

    % Remove the piece
    remove_piece(State, Piece, PartialState),

    % Recursive call
    remove_pieces(PartialState, Pieces, NewState), !.


%! remove_rokades(+State, +Rokade, -NewState)
%
%  Remove a rokade from the state
remove_rokade(State, Rokade, NewState) :-
    state:board(State, Board),
    state:currentcolor(State, CurrentColor),
    state:rokades(State, Rokades),
    state:passant(State, Passant),

    % Remove the rokade
    delete(Rokades, Rokade, NewRokades),

    % Create the new state
    NewState = state(Board, CurrentColor, NewRokades, Passant).


%! remove_rokades(+State, +Rokades, -NewState)
%
%  Remove a list of rokades from the state
remove_rokades(State, [], State).
remove_rokades(State, [Rokade | Rokades], NewState) :-

    % Remove the rokade
    remove_rokade(State, Rokade, PartialState),

    % Recursive call
    remove_rokades(PartialState, Rokades, NewState), !.


%! set_board(+State, +Board, -NewState)
%
%  Set new board value
set_board(State, Board, NewState) :-
    state:currentcolor(State, CurrentColor),
    state:rokades(State, Rokades),
    state:passant(State, Passant),

    % Create the new state
    NewState = state(Board, CurrentColor, Rokades, Passant).


%! set_passant(+State, +Passant, -NewState)
%
%  Set new passant value
set_passant(State, Passant, NewState) :-
    state:board(State, Board),
    state:currentcolor(State, CurrentColor),
    state:rokades(State, Rokades),

    % Create the new state
    NewState = state(Board, CurrentColor, Rokades, Passant).


%! set_color(+State, +Color, -NewState)
%
%  Set new color value
set_color(State, Color, NewState) :-
    state:board(State, Board),
    state:passant(State, Passant),
    state:rokades(State, Rokades),

    % Create the new state
    NewState = state(Board, Color, Rokades, Passant).


%! check(+State, +Color)
%
% If the king of the given color in-check for the given state.
% The king is now in range of attack by the opponent player.
check(State, Color) :-

    % Retrieve the king from the board
    state:king(State, Color, KingPiece), !,

    % Check if any opponent piece can attack the king of the given color
    state:can_be_attacked(KingPiece, State).


%! can_be_attacked/2(+Piece, +State)
%
%  If a given piece can be attacked in the given state.
can_be_attacked(Piece, State) :-
    piece:color(Piece, Color),
    
    % Opponent Color
    piece:opponent(Color, OpponentColor),

    % Opponent Pieces
    state:color_pieces(State, OpponentColor, OpponentPieces),

    % Helper predicate
    can_be_attacked_by_pieces(Piece, State, OpponentPieces).

%! can_be_attacked_by_pieces(+Piece, +State, +OpponentPieces)
%
%  If a given piece can be attacked by a list of opponent pieces.
can_be_attacked_by_pieces(Piece, State, [OpponentPiece | _]) :-              % Can be attacked by moves of current piece

    % Possible moves
    move:possible_moves(OpponentPiece, State, OpponentMoves),

    % Piece can be attacked by received list of moves
    can_be_attacked_by_moves(Piece, OpponentMoves).
can_be_attacked_by_pieces(Piece, State, [_ | OpponentPieces]) :- % Cannot be attacked by moves of current piece
    % Recursive call
    can_be_attacked_by_pieces(Piece, State, OpponentPieces).

%! can_be_attacked_by_moves(+Piece, +Moves)
%
%  If a given piece can be attacked in a given list of moves.
can_be_attacked_by_moves(Piece, [Move | _]) :-  % Can be attacked
    move:delete_pieces(Move, DeletePieces),

    % Piece is present inside the move
    memberchk(Piece, DeletePieces), !.
can_be_attacked_by_moves(Piece, [_ | Moves]) :- % Cannot be attacked
    
    % Recursive call
    can_be_attacked_by_moves(Piece, Moves), !.


%! checkmate_or_stalemate/1(+State)
%
%  If a given state is checkmate or stalemate for the current player.
%  Will check if the current player cannot do any more moves.
checkmate_or_stalemate(State) :-
    state:currentcolor(State, Color),

    % Find all possible positions on the board
    position:valid_positions(Positions),

    % Helper predicate
    checkmate_or_stalemate(State, Color, Positions).

%! checkmate_or_stalemate/3(+State, +Color +Positions)
%
%  Helper predicate for checkmate_or_stalemate/1
checkmate_or_stalemate(_, _, []).
checkmate_or_stalemate(State, Color, [Position | Positions]) :-     % Piece at position is of given color
    Piece = piece(Color, _, Position),

    % Piece at the current position
    piece_at_position(State, Position, Piece),

    % All possible moves for the current piece.
    move:possible_moves(Piece, State, PseudoMoves),

    % All possible valid moves for the current piece should be empty.
    move:valid_moves(State, PseudoMoves, []),

    % Recursive call.
    checkmate_or_stalemate(State, Color, Positions).
checkmate_or_stalemate(State, Color, [Position | Positions]) :-              % Piece at position is not of given color or none

    % Position is of opponent or empty
    position:empty_or_opponent_position(Position, Color, State),

    % Recursive call.
    checkmate_or_stalemate(State, Color, Positions).
