:- module(parser, []).

:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module("../state").

% Interpret quoted strings as ASCII character codes.
:- set_prolog_flag(double_quotes, codes).


%! parse_state(-State, StartColor)
%
%  Parse a chess game state.
parse_state(State) --> 
    parse_rows(8, PiecesList, RokadesList, Passant, StartColor),
    parse_final_row,
    {
        % Create a flat list of pieces
        append(PiecesList, Pieces),

        % Create a flat list of rokades
        append(RokadesList, Rokades),

        % Create state
        state:create_state(Pieces, StartColor, Rokades, Passant, State),

        % Set passant to "none" if no en-passant move was unified
        (Passant = none, ! ; true)
    }.


%! parse_rows(+Y, -Pieces, -Rokades, -StartColor)
%
%  Parse all rows in a flat list of board positions.
parse_rows(8, [Pieces | PiecesRest], [Rokades | RokadesRest], Passant, StartColor) --> % Last row
    % Parse the row
    parse_border_row(8, black, Pieces, Rokades, Passant, StartColor),

    % Recursive call
    parse_rows(7, PiecesRest, RokadesRest, Passant, StartColor), !.

parse_rows(1, [Pieces], [Rokades], Passant, StartColor) --> % First row
    % Parse the row
    parse_border_row(1, white, Pieces, Rokades, Passant, StartColor).

parse_rows(Y, [Pieces | PiecesRest], Rokades, Passant, StartColor) --> % Rows in between first & last row
    {
        YNext is Y - 1
    },

    % Parse the row
    parse_row(Y, Pieces),
    
    % Recursive call
    parse_rows(YNext, PiecesRest, Rokades, Passant, StartColor), !.


%! parse_row(-Y, -Pieces)
%
%  Parse a row of the chess board.
%  Will not parse the first/last row
parse_row(Y, Pieces) --> 
    parse_row_number(Y),
    parse_space,
    parse_pieces(1/Y, Pieces),
    parse_newline.


%! parse_border_row(+Y, +Color, -Pieces, -Rokades, -StartColor)
%
%  Parse the first/last row of the chess board.
parse_border_row(Y, Color, Pieces, Rokades, Passant, StartColor) -->
    parse_row_number(Y),
    parse_space,
    parse_pieces(1/Y, Pieces),
    parse_space,
    parse_metadata(Color, Rokades, Passant),
    parse_current_player(Y, StartColor),
    parse_newline.


%! parse_metadata(+Color, -Rokades, -Passant)
%
%  Parse metadata (possible rokades & passant posibility)
parse_metadata(Color, Rokades, Passant) -->
    "[",
    parse_rokades(Color, Rokades),
    parse_passant(Color, Passant),
    "]".


%! parse_rokades(+Color, -Rokades)
%
%  Parse a rokade notation.
%  
%  This is verbose on purpose to allow re-use of the parser for generating output.
parse_rokades(Color, [LongRokade, ShortRokade]) --> % Both rokades
    parse_rokade_piece(Color, long, LongRokade),
    parse_rokade_piece(Color, short, ShortRokade).

parse_rokades(Color, [LongRokade]) --> % Only first rokade
    parse_rokade_piece(Color, long, LongRokade),
    parse_space.

parse_rokades(Color, [ShortRokade]) --> % Only second rokade
    parse_space,
    parse_rokade_piece(Color, short, ShortRokade).

parse_rokades(_, []) --> % No rokades
    parse_space,
    parse_space.

%! parse_rokade_piece(+Color, +RokadeType, -Rokades)
%
%  Parse a single piece of the rokade notation
parse_rokade_piece(Color, long, Rokade) --> % Large
    parse_piece(Color, queen), 

    % Create the rokade
    {
        Rokade = rokade(Color, long)
    }.

parse_rokade_piece(Color, short, Rokade) --> % Short
    parse_piece(Color, king),

    % Create the rokade
    {
        Rokade = rokade(Color, short)
    }.


%! parse_passant(+Color, -Passant)
%
%  Parse passant possibility
parse_passant(Color, passant(Color, X/Y)) --> % En-passant possible
    parse_column_number(X),
    parse_row_number(Y).
parse_passant(_, _) --> [].                   % En-passant not possible


%! parse_passant_position(-X/-Y)
%
%  Parse passant possibility position
parse_passant_position(X/Y) -->          % En-passant possible
    parse_column_number(X),
    parse_row_number(Y).

parse_passant_position(_) --> [].        % En-passant not possible


%! parse_current_player(+Y, -StartColor)
%
%  Parse a current player symbol or nothing
parse_current_player(8, black) --> "☚".
parse_current_player(1, white) --> "☚".
parse_current_player(_, _)     --> "".


%! parse_final_row()
%
%  Parse the final row of the board.
%  This row does not contain any extra information.
parse_final_row --> 
    parse_space,
    parse_space,
    "abcdefgh",
    parse_newline_or_nothing.
    

%! parse_row_number(+RowNumber)
%
%  Parse a row number between 1 and 8.
parse_row_number(RowNumber) -->
    {
        % RowNumber must be a valid Y-value
        % (this is here to allow for re-using the parser as output writer)
        between(1, 8, RowNumber)
    },
    integer(RowNumber).


%! parse_column_number(+ColumnNumber)
%
%  Parse a column number between 1 and 8.
parse_column_number(1) --> "a".
parse_column_number(2) --> "b".
parse_column_number(3) --> "c".
parse_column_number(4) --> "d".
parse_column_number(5) --> "e".
parse_column_number(6) --> "f".
parse_column_number(7) --> "g".
parse_column_number(8) --> "h".


%! parse_space()
%
%  Parse a single space.
parse_space --> " ".


%! parse_newline()
%
% Parse a single newline.
parse_newline --> "\n".


%! parse_newline_or_nothing()
%
% Parse a single newline or nothing
parse_newline_or_nothing --> parse_newline.
parse_newline_or_nothing --> "".


%! parse_pieces(-Pieces)
%
%  Parse a row of pieces.
%  Will stop parsing when a newline is detected

% Piece: taken position
parse_pieces(X/Y, [Piece | Pieces]) -->
    {
        XNext is X + 1,

        % Create the piece
        Piece = piece(Color, Type, X/Y),

        % X must be a valid X-value
        % (this is here to allow for re-using the parser as output writer)
        between(1, 8, X)
    },

    parse_piece(Color, Type), 
    parse_pieces(XNext/Y, Pieces).

% Space: empty position
parse_pieces(X/Y, Pieces) -->
    {
        XNext is X + 1,

        % X must be a X-value
        % (this is here to allow for re-using the parser as output writer)
        between(1, 8, X)
    },

    parse_space,
    parse_pieces(XNext/Y, Pieces).

parse_pieces(_, []) --> [].


%! parse_piece(-Color, -Type)
%
%  Parse a single piece.
parse_piece(white, king)   --> "\u2654". % White king
parse_piece(white, queen)  --> "\u2655". % White queen
parse_piece(white, tower)  --> "\u2656". % White tower
parse_piece(white, bishop) --> "\u2657". % White tower
parse_piece(white, horse)  --> "\u2658". % White horse
parse_piece(white, pawn)   --> "\u2659". % White pawn

parse_piece(black, king)   --> "\u265A". % Black king
parse_piece(black, queen)  --> "\u265B". % Black queen
parse_piece(black, tower)  --> "\u265C". % Black tower
parse_piece(black, bishop) --> "\u265D". % Black tower
parse_piece(black, horse)  --> "\u265E". % Black horse
parse_piece(black, pawn)   --> "\u265F". % Black pawn