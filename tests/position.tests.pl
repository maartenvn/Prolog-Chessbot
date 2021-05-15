:- begin_tests(position).
:- use_module("../src/state").
:- use_module("../src/position").
:- use_module("../src/util/utils").


% horse_position/3 with no other pieces in radius
test(horse_position__empty) :-
    Horse = piece(white, horse, 4/4),
    state:create_state([Horse], white, [], none, State),

    % Expected positions
    ExpectedPositions = [3/2, 3/6, 2/3, 2/5, 6/3, 6/5, 5/2, 5/6],

    % Received positions
    findall(Position, position:horse_position(Horse, State, Position), ReceivedPositions),

    % Assert
    utils:list_equals(ExpectedPositions, ReceivedPositions).

% horse_position/3 with other pieces in radius
test(horse_position__other) :-
    Horse = piece(white, horse, 4/4),
    Opponent = piece(black, pawn, 3/6),
    Player = piece(white, pawn, 2/5),
    state:create_state([Horse, Opponent, Player], white, [], none, State),

    % Expected positions
    ExpectedPositions = [3/2, 3/6, 2/3, 6/3, 6/5, 5/2, 5/6],

    % Received positions
    findall(Position, position:horse_position(Horse, State, Position), ReceivedPositions),

    % Assert
    utils:list_equals(ExpectedPositions, ReceivedPositions).


% square_position/3 with no other pieces in radius
test(square_position__empty) :-
    King = piece(white, king, 2/2),
    state:create_state([King], white, [], none, State),

    % Expected positions
    ExpectedPositions = [1/1, 2/1, 3/1, 3/2, 3/3, 2/3, 1/3, 1/2],

    % Received positions
    findall(Position, position:square_position(King, State, Position), ReceivedPositions),

    % Assert
    utils:list_equals(ExpectedPositions, ReceivedPositions).

% square_position/3 with other pieces in radius
test(square_position__empty) :-
    King = piece(white, king, 2/2),
    Opponent = piece(black, pawn, 1/1),
    Player = piece(white, pawn, 3/1),
    state:create_state([King, Opponent, Player], white, [], none, State),

    % Expected positions
    ExpectedPositions = [1/1, 2/1, 3/2, 3/3, 2/3, 1/3, 1/2],

    % Received positions
    findall(Position, position:square_position(King, State, Position), ReceivedPositions),

    % Assert
    utils:list_equals(ExpectedPositions, ReceivedPositions).


% valid_position/1
test(valid_position) :-

    % Assert
    not(position:valid_position(9/1)),
    not(position:valid_position(1/9)),
    not(position:valid_position(0/1)),
    not(position:valid_position(1/0)).


% empty_position/2
test(empty_position) :-
    Piece = piece(white, pawn, 1/1),
    state:create_state([Piece], white, [], none, State),

    % Assert
    not(position:empty_position(1/1, State)).


% opponent_position/2: opponent present
test(opponent_position__opponent) :-
    Piece = piece(black, pawn, 2/1),
    state:create_state([Piece], white, [], none, State),

    % Assert
    position:opponent_position(2/1, white, State).


% opponent_position/2: empty position
test(opponent_position__opponent) :-
    Piece = piece(black, pawn, 2/1),
    state:create_state([Piece], white, [], none, State),

    % Assert
    not(position:opponent_position(1/1, black, State)).


% opponent_position/2: color piece of player at location position
test(opponent_position__opponent) :-
    Piece = piece(white, pawn, 2/1),
    state:create_state([], white, [], none, State),

    % Assert
    not(position:opponent_position(2/1, white, State)).


% empty_or_opponent_position/3: empty
test(empty_or_opponent_position__empty) :-
    state:create_state([], white, [], none, State),

    % Assert
    position:empty_or_opponent_position(1/1, white, State).


% empty_or_opponent_position/3: opponent
test(empty_or_opponent_position__opponent) :-
    Piece = piece(white, pawn, 2/1),
    state:create_state([Piece], white, [], none, State),

    % Assert
    position:empty_or_opponent_position(2/1, black, State).


% empty_or_opponent_position/3: player piece
test(empty_or_opponent_position__opponent) :-
    Piece = piece(black, pawn, 2/1),
    state:create_state([Piece], white, [], none, State),

    % Assert
    not(position:empty_or_opponent_position(2/1, black, State)).


% empty_between_positions/3: same coordinate
test(empty_between_positions__same) :- 
    state:create_state([], white, [], none, State),
    
    % Assert
    position:empty_between_positions(1/1, 1/1, State).


% empty_between_positions/3: empty
test(empty_between_positions__empty) :- 
    PieceLeft = piece(black, pawn, 1/1),
    PieceRight = piece(black, pawn, 3/1),
    state:create_state([PieceLeft, PieceRight], white, [], none, State),

    % Assert
    position:empty_between_positions(1/1, 3/1, State).


% empty_between_positions/3: not empty
test(empty_between_positions__notempty) :- 
    PieceLeft = piece(black, pawn, 1/1),
    PieceMiddle = piece(black, pawn, 2/1),
    PieceRight = piece(black, pawn, 4/1),
    state:create_state([PieceLeft, PieceMiddle, PieceRight], white, [], none, State),

    % Assert
    not(position:empty_between_positions(1/1, 4/1, State)).

:- end_tests(position).