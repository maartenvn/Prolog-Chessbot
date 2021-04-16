:- module(state, []).

:- use_module("moves").


%! pieces(+State, -Pieces)
%
%  Extract the pieces from the given state.
pieces(state(Pieces, _, _, _), Pieces).


%! currentcolor(+State, -CurrentColor)
%
%  Extract the current color (the player that can do the current move) from the given state.
currentcolor(state(_, CurrentColor, _, _), CurrentColor).


%! nextcolor(+State, -NextColor)
%
%  Extract the next color (the player that can do the next move) from the given state.
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


%! position_piece(+Position, +State, -Piece)
%
%  Piece at a given position for the given state.
position_piece(Position, State, piece(Color, Type, Position)) :-
    pieces(State, Pieces),
    
    % Select the piece from the pieces list
    select(piece(Color, Type, Position), Pieces, _), !.


%! color_pieces(+Color, +State, -ColorPieces)
%
%  Unify all pieces for a given Color from the given state.
color_pieces(Color, State, ColorPieces) :-
    state:pieces(State, Pieces),

    % Extract the pieces that match the given color.
    include(piece_color(Color), Pieces, ColorPieces).


%! piece_color(-Color, +Piece)
%
%  Extract the color of a piece
%  This can be used as a predicate in predicates like "include"
piece_color(Color, piece(Color, _, _)).