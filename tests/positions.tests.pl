:- begin_tests(lists).
:- use_module(library(lists)).
:- ["../src/positions"].

% possible_positions: test if the king has the correct possible positions
test(possible_moves__king) :-

    % List with possible positions
    PossiblePositions = [4/4, 4/5, 4/6, 5/4, 5/6, 6/4, 6/5, 6/6],

    % Assert
    possible_positions(piece(white, king, 5/5), [], PossiblePositions).

% checkmate: test if the king is checkmate for the given board
test(checkmate__true) :-

    % Assert
    checkmate([piece(white, king, 1/1), piece(black, king, 8/8), piece(black, tower, 2/7), piece(black, queen, 2/2)], white).

% checkmate: test if the king is not checkmate for the given board
test(checkmate__false) :-

    % Assert
    not(checkmate([piece(white, king, 1/1), piece(black, king, 8/8), piece(black, tower, 2/7), piece(black, queen, 2/2)], black)).

:- end_tests(lists).