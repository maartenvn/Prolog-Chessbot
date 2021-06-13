:- begin_tests(move).
:- use_module("../src/move").
:- use_module("../src/util/utils").


% possible_moves/3: for king
test(possible_moves__king, [nondet]) :-
    King = piece(white, king, 2/2),
    state:create_state([King], white, [], none, State),

    % Expected moves
    ExpectedMoves =  [
        move([King], [piece(white, king, 3/3)]),
        move([King], [piece(white, king, 3/2)]),
        move([King], [piece(white, king, 3/1)]),
        move([King], [piece(white, king, 2/3)]),
        move([King], [piece(white, king, 2/1)]),
        move([King], [piece(white, king, 1/3)]),
        move([King], [piece(white, king, 1/2)]),
        move([King], [piece(white, king, 1/1)])
    ],

    % Received moves
    move:possible_moves(King, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for queen
test(possible_moves__queen, [nondet]) :-
    Queen = piece(white, queen, 2/2),
    state:create_state([Queen], white, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Queen], [piece(white, queen, 3/2)]),
        move([Queen], [piece(white, queen, 4/2)]),
        move([Queen], [piece(white, queen, 5/2)]),
        move([Queen], [piece(white, queen, 6/2)]),
        move([Queen], [piece(white, queen, 7/2)]),
        move([Queen], [piece(white, queen, 8/2)]),
        move([Queen], [piece(white, queen, 1/2)]),
        move([Queen], [piece(white, queen, 2/3)]),
        move([Queen], [piece(white, queen, 2/4)]),
        move([Queen], [piece(white, queen, 2/5)]),
        move([Queen], [piece(white, queen, 2/6)]),
        move([Queen], [piece(white, queen, 2/7)]),
        move([Queen], [piece(white, queen, 2/8)]),
        move([Queen], [piece(white, queen, 2/1)]),
        move([Queen], [piece(white, queen, 3/3)]),
        move([Queen], [piece(white, queen, 4/4)]),
        move([Queen], [piece(white, queen, 5/5)]),
        move([Queen], [piece(white, queen, 6/6)]),
        move([Queen], [piece(white, queen, 7/7)]),
        move([Queen], [piece(white, queen, 8/8)]),
        move([Queen], [piece(white, queen, 1/3)]),
        move([Queen], [piece(white, queen, 3/1)]),
        move([Queen], [piece(white, queen, 1/1)])
    ],

    % Received moves
    move:possible_moves(Queen, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for tower
test(possible_moves__tower, [nondet]) :-
    Tower = piece(white, tower, 2/2),
    state:create_state([Tower], white, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Tower], [piece(white, tower, 3/2)]),
        move([Tower], [piece(white, tower, 4/2)]),
        move([Tower], [piece(white, tower, 5/2)]),
        move([Tower], [piece(white, tower, 6/2)]),
        move([Tower], [piece(white, tower, 7/2)]),
        move([Tower], [piece(white, tower, 8/2)]),
        move([Tower], [piece(white, tower, 1/2)]),
        move([Tower], [piece(white, tower, 2/3)]),
        move([Tower], [piece(white, tower, 2/4)]),
        move([Tower], [piece(white, tower, 2/5)]),
        move([Tower], [piece(white, tower, 2/6)]),
        move([Tower], [piece(white, tower, 2/7)]),
        move([Tower], [piece(white, tower, 2/8)]),
        move([Tower], [piece(white, tower, 2/1)])
    ],

    % Received moves
    move:possible_moves(Tower, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for bishop
test(possible_moves__bishop, [nondet]) :-
    Bishop = piece(white, bishop, 2/2),
    state:create_state([Bishop], white, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Bishop], [piece(white, bishop, 3/3)]),
        move([Bishop], [piece(white, bishop, 4/4)]),
        move([Bishop], [piece(white, bishop, 5/5)]),
        move([Bishop], [piece(white, bishop, 6/6)]),
        move([Bishop], [piece(white, bishop, 7/7)]),
        move([Bishop], [piece(white, bishop, 8/8)]),
        move([Bishop], [piece(white, bishop, 1/3)]),
        move([Bishop], [piece(white, bishop, 3/1)]),
        move([Bishop], [piece(white, bishop, 1/1)])
    ],

    % Received moves
    move:possible_moves(Bishop, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for horse
test(possible_moves__horse, [nondet]) :-
    Horse = piece(white, horse, 5/5),
    state:create_state([Horse], white, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Horse], [piece(white, horse, 3/6)]),
        move([Horse], [piece(white, horse, 3/4)]),
        move([Horse], [piece(white, horse, 7/6)]),
        move([Horse], [piece(white, horse, 7/4)]),
        move([Horse], [piece(white, horse, 6/3)]),
        move([Horse], [piece(white, horse, 6/7)]),
        move([Horse], [piece(white, horse, 4/3)]),
        move([Horse], [piece(white, horse, 4/7)])
    ],

    % Received moves
    move:possible_moves(Horse, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for white pawn, forward & en-passant
test(possible_moves__pawn_white_forward_passant, [nondet]) :-
    Pawn = piece(white, pawn, 3/2),
    state:create_state([Pawn], white, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Pawn], [piece(white, pawn, 3/3)]), 
        move([Pawn], [piece(white, pawn, 3/4)])
    ],

    % Received moves
    move:possible_moves(Pawn, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for white pawn, forward
test(possible_moves__pawn_white_forward, [nondet]) :-
    Pawn = piece(black, pawn, 3/3),
    state:create_state([Pawn], black, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Pawn], [piece(black, pawn, 3/2)])
    ],

    % Received moves
    move:possible_moves(Pawn, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for white pawn, diagonal
test(possible_moves__pawn_white_diagonal, [nondet]) :-
    Pawn = piece(white, pawn, 3/3),
    OpponentLeft = piece(black, pawn, 2/4),
    OpponentRight = piece(black, pawn, 4/4),
    state:create_state([Pawn, OpponentLeft, OpponentRight], white, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Pawn], [piece(white, pawn, 3/4)]),
        move([Pawn, OpponentLeft], [piece(white, pawn, 2/4)]),
        move([Pawn, OpponentRight], [piece(white, pawn, 4/4)])
    ],

    % Received moves
    move:possible_moves(Pawn, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).

% possible_moves/3: for white pawn, promition, with attack possible or just forward
test(possible_moves__pawn_white_promition, [nondet]) :-
    Pawn = piece(white, pawn, 2/7),
    OpponentLeft = piece(black, pawn, 1/8),
    OpponentRight = piece(black, pawn, 3/8),
    state:create_state([Pawn, OpponentLeft, OpponentRight], white, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Pawn], [piece(white, queen, 2/8)]), 
        move([Pawn], [piece(white, horse, 2/8)]), 
        move([Pawn], [piece(white, tower, 2/8)]), 
        move([Pawn], [piece(white, bishop, 2/8)]), 
        move([Pawn, OpponenLeft], [piece(white, queen, 1/8)]), 
        move([Pawn, OpponenLeft], [piece(white, horse, 1/8)]), 
        move([Pawn, OpponenLeft], [piece(white, tower, 1/8)]), 
        move([Pawn, OpponenLeft], [piece(white, bishop, 1/8)]), 
        move([Pawn, OpponenRight], [piece(white, queen, 3/8)]), 
        move([Pawn, OpponenRight], [piece(white, horse, 3/8)]), 
        move([Pawn, OpponenRight], [piece(white, tower, 3/8)]), 
        move([Pawn, OpponenRight], [piece(white, bishop, 3/8)])
    ],

    % Received moves
    move:possible_moves(Pawn, State, ReceivedMoves),
    
    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for black pawn, forward & en-passant
test(possible_moves__pawn_black_forward_passant, [nondet]) :-
    Pawn = piece(black, pawn, 5/7),
    state:create_state([Pawn], black, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Pawn], [piece(black, pawn, 5/6)]), 
        move([Pawn], [piece(black, pawn, 5/5)])
    ],

    % Received moves
    move:possible_moves(Pawn, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for black pawn, forward
test(possible_moves__pawn_black_forward, [nondet]) :-
    Pawn = piece(black, pawn, 3/3),
    state:create_state([Pawn], black, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Pawn], [piece(black, pawn, 3/2)])
    ],

    % Received moves
    move:possible_moves(Pawn, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).


% possible_moves/3: for black pawn, diagonal
test(possible_moves__pawn_black_diagonal, [nondet]) :-
    Pawn = piece(black, pawn, 6/3),
    OpponentLeft = piece(white, pawn, 5/2),
    OpponentRight = piece(white, pawn, 7/2),
    state:create_state([Pawn, OpponentLeft, OpponentRight], black, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Pawn], [piece(black, pawn, 6/2)]),
        move([Pawn, OpponentLeft], [piece(black, pawn, 5/2)]),
        move([Pawn, OpponentRight], [piece(black, pawn, 7/2)])
    ],

    % Received moves
    move:possible_moves(Pawn, State, ReceivedMoves),

    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).

% possible_moves/3: for black pawn, promition, with attack possible or just forward
test(possible_moves__pawn_black_promition, [nondet]) :-
    Pawn = piece(black, pawn, 3/2),
    OpponentLeft = piece(white, pawn, 2/1),
    OpponentRight = piece(white, pawn, 4/1),
    state:create_state([Pawn, OpponentLeft, OpponentRight], black, [], none, State),

    % Expected moves
    ExpectedMoves = [
        move([Pawn], [piece(black, queen, 3/1)]), 
        move([Pawn], [piece(black, horse, 3/1)]), 
        move([Pawn], [piece(black, tower, 3/1)]), 
        move([Pawn], [piece(black, bishop, 3/1)]), 
        move([Pawn, OpponenLeft], [piece(black, queen, 2/1)]), 
        move([Pawn, OpponenLeft], [piece(black, horse, 2/1)]), 
        move([Pawn, OpponenLeft], [piece(black, tower, 2/1)]), 
        move([Pawn, OpponenLeft], [piece(black, bishop, 2/1)]), 
        move([Pawn, OpponenRight], [piece(black, queen, 4/1)]), 
        move([Pawn, OpponenRight], [piece(black, horse, 4/1)]), 
        move([Pawn, OpponenRight], [piece(black, tower, 4/1)]), 
        move([Pawn, OpponenRight], [piece(black, bishop, 4/1)])
    ],

    % Received moves
    move:possible_moves(Pawn, State, ReceivedMoves),
    
    % Assert
    utils:list_equals(ExpectedMoves, ReceivedMoves).

:- end_tests(move).