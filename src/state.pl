:- module(state, []).

:- use_module("moves").
:- use_module("pieces").
:- use_module("positions").


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


%! pieces/2(+State, -Pieces)
%
%  List of pieces that are currently on the board
pieces(State, Pieces) :-

    % Find all possible positions on the board
    findall(X/Y, positions:valid_position(X/Y), Positions),
    
    % Helper predicate
    pieces(State, Positions, Pieces).


%! pieces/3(+State, +Positions, -Pieces)
%
%  Helper predicate for pieces/3
pieces(State, [Position | Positions], [Piece | Pieces]) :- % Piece at current position is not "none"
    
    % Piece at the current position
    piece_at_position(State, Position, Piece),

    % Piece must not be none
    Piece \= none,

    % Recursive call
    pieces(State, Positions, Pieces), !.
    
pieces(State, [_ | Positions], Pieces) :-                  % Piece at current position is "none"

    % Recursive call
    pieces(State, Positions, Pieces), !.

pieces(_, [], []) :- !.                                    % Base case


%! color_pieces/3(+State, +Color, -ColorPieces)
%
%  Unify all pieces for a given Color from the given state.
color_pieces(State, Color, ColorPieces) :-
    % Find all possible positions on the board
    positions:valid_positions(Positions),

    color_pieces(State, Color, Positions, ColorPieces).


%! color_pieces/4(+State, +Color, +Positions, -ColorPieces)
%
%  Helper predicate for color_pieces/3
color_pieces(State, Color, [Position | Positions], [ColorPiece | ColorPieces]) :- % Piece at current position is of the given color
    
    % Piece at the current position
    piece_at_position(State, Position, ColorPiece),

    % Color must match
    pieces:color(ColorPiece, Color),

    % Recursive call
    color_pieces(State, Color, Positions, ColorPieces), !.

color_pieces(State, Color, [_ | Positions], ColorPieces) :-                      % Piece at current position is "none" or not of the given color
    
    % Recursive call
    color_pieces(State, Color, Positions, ColorPieces), !.

color_pieces(_, _, [], []) :- !.                                                 % Base case


%! piece_at_position(+State, +X/+Y, -Piece).
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
%  TODO: ask if we can use setarg.
%  TODO: this predicate can improve performance (ask about duplicate terms)
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
    pieces:position(Piece, X/Y),

    % Set the piece at the given position
    set_piece_at_position(State, Piece, X/Y, NewState).


%! set_pieces(+State, +Pieces, +NewState)
%
%  Set a list of pieces on the board in a given state.
set_pieces(State, [Piece | Pieces], NewState) :-
   
    % Set the piece
    set_piece(State, Piece, PartialState),
    
    % Recursive call
    set_pieces(PartialState, Pieces, NewState), !.
set_pieces(State, [], State) :- !.


%! remove_piece(+State, +Piece, -NewState)
%
%  Remove a piece from the board in a given state.
remove_piece(State, Piece, NewState) :-
    pieces:position(Piece, X/Y),

    % Set the piece position to none
    set_piece_at_position(State, none, X/Y, NewState).


%! remove_pieces(+State, +Pieces, -NewState)
%
%  Remove a list of pieces from the board
remove_pieces(State, [Piece | Pieces], NewState) :-

    % Remove the piece
    remove_piece(State, Piece, PartialState),

    % Recursive call
    remove_pieces(PartialState, Pieces, NewState), !.
remove_pieces(State, [], State) :- !.


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
remove_rokades(State, [Rokade | Rokades], NewState) :-

    % Remove the rokade
    remove_rokade(State, Rokade, PartialState),

    % Recursive call
    remove_rokades(PartialState, Rokades, NewState), !.
remove_rokades(State, [], State) :- !.


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
    pieces(State, Pieces),

    % King Piece
    KingPiece = piece(Color, king, _),

    % Retrieve the king from the board
    select(KingPiece, Pieces, _), !,

    % Check if any opponent piece can attack the king of the given color
    can_be_attacked(KingPiece, State).


%! can_be_attacked/2(+Piece, +State)
%
%  If a given piece can be attacked in the given state.
%  TODO: can be more efficient by only evaluating moves when necessary!!!!
can_be_attacked(Piece, State) :-
    pieces:color(Piece, Color),
    
    % Opponent Color
    positions:opponent(Color, OpponentColor),

    % All possible moves by the opponent
    moves:all_possible_moves(OpponentColor, State, Moves),

    % Helper predicate
    can_be_attacked_for_moves(Piece, Moves).


%! can_be_attacked_for_moves(+Piece, +Moves)
%
%  If a given piece can be attacked in a given list of moves.
can_be_attacked_for_moves(Piece, [Move | _]) :-  % Can be attacked
    Move = move(DeletePieces, _, _, _),

    % Piece is present inside the move
    memberchk(Piece, DeletePieces), !.

can_be_attacked_for_moves(Piece, [_ | Moves]) :- % Cannot be attacked
    
    % Recursive call
    can_be_attacked_for_moves(Piece, Moves), !.
