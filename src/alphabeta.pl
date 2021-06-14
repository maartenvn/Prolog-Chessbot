:- module(alphabeta, []).

:- use_module("move").
:- use_module("state").
:- use_module("position").
:- use_module("piece").


%! alphabeta(+Player, +CurrentState, +TraversedDepth, +MaxDepth, +LowerBound, +UpperBound, -BestState, -BestScore)
%  
%  Alpha/beta pruning to determin the next best move.
%  Depth will specify the max recursion depth.
alphabeta(Player, CurrentState, MaxDepth, MaxDepth, _, _, CurrentState, BestScore) :-                       % Leaf: maximum depth is reached
   
    % Calculate the score for the current state
    score(Player, CurrentState, MaxDepth, MaxDepth, BestScore).
    
alphabeta(Player, CurrentState, TraversedDepth, MaxDepth, LowerBound, UpperBound, BestState, BestScore) :-  % Continue building the game tree

    % Determin all possible next states for the current state
    state:all_possible_states(CurrentState, NextStates),

    % Next States must not be empty (otherwise there is a checkmate or a stalemate)
    NextStates \= [],

    % Decrement the depth
    NextTraversedDepth is TraversedDepth + 1,

    % Find the best possible move
    best(Player, NextStates, NextTraversedDepth, MaxDepth, LowerBound, UpperBound, BestState, BestScore), !.

% This branch will only be reached if the above 2 variants of the predicate fail.
% This is only the case when all_possible_states is empty,
% which is checkmate for the current player.
alphabeta(Player, CurrentState, TraversedDepth, MaxDepth, _, _, CurrentState, BestScore) :- % Leaf: a player is checkmate
    state:currentcolor(CurrentState, CheckmatePlayer),

    % Make sure the king is check (otherwise a stalemate is reached)
    state:check(CurrentState, CheckmatePlayer),

    % Calculate the score fot the current state.
    score_checkmate(Player, CheckmatePlayer, TraversedDepth, MaxDepth, BestScore).

alphabeta(Player, CurrentState, _, _, _, _, none, BestScore) :-                             % Leaf: a player is stalemate
    state:currentcolor(CurrentState, StalematePlayer),

    % Calculate the score fot the current state.
    score_stalemate(Player, StalematePlayer, BestScore).


%! best(+Player, +States, +TraversedDepth, +MaxDepth, +LowerBound, +UpperBound, -BestState, -BestScore)
%
%  Best state in a given list of states.
best(_, [], _, _, _, _, _, _).                                                                                     % Base case

best(Player, [State], TraversedDepth, MaxDepth, LowerBound, UpperBound, State, Score) :-                           % Single state, return state

    % Do minimax for the current state
    alphabeta(Player, State, TraversedDepth, MaxDepth, LowerBound, UpperBound, _, Score) , !.

best(Player, [State | OtherStates], TraversedDepth, MaxDepth, LowerBound, UpperBound, BestState, BestScore) :-     % Multiple states, determin best state

    % Do minimax for the current state
    alphabeta(Player, State, TraversedDepth, MaxDepth, LowerBound, UpperBound, _, Score),

    % Cut or continue evaluation
    cut(Player, State, Score, OtherStates, TraversedDepth, MaxDepth, LowerBound, UpperBound, BestState, BestScore).
    

%! cut(+Player, +State, +Score, +OtherStates, +TraversedDepth, +MaxDepth, +LowerBound, +UpperBound, -BestState, -BestScore)
%
%  Cut of branches that will never lead to a result (Alpha/Beta-pruning)
cut(Player, State, Score, _, _, _, _, UpperBound, State, Score) :-                                                   % Maximizing player, cut-off
    max(State, Player),

    % Cut-off the branch if the score is larger than the upperbound
    Score >= UpperBound.

cut(Player, State, Score, _, _, _, LowerBound, _, State, Score) :-                                                   % Minimizing player, cut-off
    min(State, Player),

    % Cut-off the branch if the score is larger than the upperbound
    Score =< LowerBound.

cut(Player, State1, Score1, OtherStates, TraversedDepth, MaxDepth, LowerBound, UpperBound, BestState, BestScore) :-  % Continue evaluation

    % Update upper/lower bound
    update_bounds(Player, State1, Score1, LowerBound, UpperBound, NewLowerBound, NewUpperBound),

    % Continue evaluation of the other states
    best(Player, OtherStates, TraversedDepth, MaxDepth, NewLowerBound, NewUpperBound, State2, Score2),

    % Determin the best state of the 2 states
    best_of(Player, State1, State2, Score1, Score2, BestState, BestScore).


%! update_bounds(+Player, +State, +Score, +LowerBound, +UpperBound, -NewLowerBound, -NewUpperBound)
%
%  Update the upper & lowerbound, if necessary

% Update the lowerbound to the current score if:
% * Current player is maximizing player
% * Score is larger than the lowerbound
update_bounds(Player, State, Score, LowerBound, UpperBound, Score, UpperBound) :-
    max(State, Player),

    % Score must be larger than the lowerbound.
    Score > LowerBound.

% Update the upperbound to the current score if:
% * Current player is minimizing player
% * Score is smaller than the upperbound
update_bounds(Player, State, Score, LowerBound, UpperBound, LowerBound, Score) :-
    min(State, Player),

    % Score must be larger than the lowerbound.
    Score < UpperBound.

% Base case
update_bounds(_, _, _, LowerBound, UpperBound, LowerBound, UpperBound).


%! best_of(+Player, +State1, +State2, +Score1, +Score2, -BestState, -BestScore)
%
%  Best state between 2 states, based on their scores.
best_of(Player, State1, _, Score1, Score2, BestState, BestScore) :- % Maximizing player (Score 1 is largest)
    max(State1, Player),

    % Score 1 is largest
    Score1 >= Score2,

    % Update best state
    BestState = State1,
    BestScore = Score1.

best_of(Player, _, State2, Score1, Score2, BestState, BestScore) :- % Maximizing player (Score 2 is largest)
    max(State2, Player),

    % Score 2 is largest
    Score1 < Score2,

    % Update best state
    BestState = State2,
    BestScore = Score2.

best_of(Player, State1, _, Score1, Score2, BestState, BestScore) :- % Minimizing player (Score 1 is smallest)
    min(State1, Player),

    % Score 1 is smallest
    Score1 =< Score2,

    % Update best state
    BestState = State1,
    BestScore = Score1.

best_of(Player, _, State2, Score1, Score2, BestState, BestScore) :- % Minimizing player (Score 2 is smallest)
    min(State2, Player),

    % Score 2 is smallest
    Score1 > Score2,

    % Update best state
    BestState = State2,
    BestScore = Score2.


%! max(+State, +Player)
%
%  If the state is for the maximizing player
%  Since the state contains the player that can do the next move, the currentcolor must be different from the player.
max(State, Player) :-
    state:currentcolor(State, CurrentPlayer),
    CurrentPlayer \== Player.


%! min(+State, +Player)
%
%  If the state is for the minimizing player
%  Since the state contains the player that can do the next move, the currentcolor must be the same as the player.
min(State, Player) :-
    state:currentcolor(State, CurrentPlayer),
    CurrentPlayer == Player.


%! score(+Player, +State, +TraversedDepth, +MaxDepth, -Score)
%
%  Score for a given state.
%
%  This scoring predicate is a symmetric evaluation function that will score the current state of the board.
%  It does not keep track of previous states or boards and just evaluates the current state, as is.
%
%  - Each piece will receive a value based on it's importance in the game
%  - Pawns will be encouraged to advance. If pawns keep stuck on the central row, they will protect the king, but block all other pieces from advancing.
%
%  To score a state we subtract the player's score with the opponent's score
%  This way boards with a large difference will receive a higher score
%  => If the player has a higher score, the overal score will be positive
%  => If the opponent has a higher score, the overal score will be negative

score(Player, State, TraversedDepth, MaxDepth, Score) :- % Checkmate or stalemate

    % Make sure the player cannot do any more moves
    state:all_possible_states(State, []),

    % Checkmate or stalemate
    score_checkmate_or_stalemate(Player, State, TraversedDepth, MaxDepth, Score).

score(Player, State, _, _, Score) :-                     % Symmetric evaluation scoring function

    % Score for the player
    score_player(Player, State, PlayerScore),

    % Score for the opponent
    piece:opponent(Player, OpponentPlayer),
    score_player(OpponentPlayer, State, OpponentScore),

    % Calculate the state score
    Score is PlayerScore - OpponentScore.

%! score_player(+Player, +State, -Score)
%
%  Score for a given state for a given player.
score_player(Player, State, Score) :-
    % Get the pieces for the given player
    state:color_pieces(State, Player, ColorPieces),

    % Evaluate every piece and add it's score to the scores
    score_pieces(ColorPieces, PiecesScore),

    % Add all the scores together
    Score = PiecesScore.


%! score_checkmate_or_stalemate(+Player, +State, +TraversedDepth, +MaxDepth, -Score)
%
%  Score when a given player is either checkmate or stalemate.
%  If the player is checkmate, return the checkmate score.
%  If the player is stalemate, return the stalemate score.
score_checkmate_or_stalemate(Player, State, TraversedDepth, MaxDepth, Score) :-   % Checkmate
    state:currentcolor(State, CurrentPlayer),

    % Make sure the king is check (otherwise a stalemate is reached)
    state:check(State, CurrentPlayer),

    % Checkmate score
    score_checkmate(Player, CurrentPlayer, TraversedDepth, MaxDepth, Score).

score_checkmate_or_stalemate(Player, State, _, _, Score) :-                       % Stalemate
    state:currentcolor(State, CurrentPlayer),

    % Checkmate score
    score_stalemate(Player, CurrentPlayer, Score).


%! score_checkmate(+Player, +CheckmatePlayer, +TraversedDepth, +MaxDepth, -Score)
%
%  Score when a given state is checkmate.
score_checkmate(Player, CheckmatePlayer, TraversedDepth, MaxDepth, Score) :-
    (
        Player == CheckmatePlayer, PartialScore = -10000
        ;
        PartialScore = 10000
    ),

    % Apply a depth penalty to the checkmate.
    % This will make sure the chosen nextmove will be based on the branch with the quickest possible checkmate.
    ScorePenalty is MaxDepth - TraversedDepth, % This value will become smaller when the traversed depth will increase
    Score is PartialScore + ScorePenalty.


%! score_stalemate(+Player, +StalematePlayer, -Score)
%
%  Score when a given state is stalemate.
score_stalemate(_, _, 0).


%! score_pieces(+Pieces, -Score)
%
%  Based on Hans Berliner's System.
%  Score a set of pieces.
score_pieces([Piece | Pieces], Score) :-

    % Score for the current piece
    score_piece(Piece, PieceScore),
    
    % Recursive call
    score_pieces(Pieces, PiecesScore),

    % Add the scores together
    Score is PieceScore + PiecesScore.
score_pieces([], 0).


%! score_piece(+Piece, -Score)
%
%  Score a single piece.
%  Scores are based on the values recommended by Hans Berliner's system (World Correspondence Chess Champion)

% Queen
score_piece(piece(_, queen, _), 8.8).

% Tower
score_piece(piece(_, tower, _), 5.1).

% Bishop
score_piece(piece(_, bishop, _), 3.33).

% Horse
score_piece(piece(_, horse, _), 3.2).

% Pawn
score_piece(piece(white, pawn, X/Y), Score) :-    % white: Get score from pawn table
    
    % Scoring table
    score_pawn_table(ScoringTable),

    % Row
    nth0(Y, ScoringTable, Row),

    % Score
    nth0(X, Row, Score).

score_piece(piece(black, pawn, X/Y), Score) :-    % black: Get score from pawn table
    XRev is 8 - X,
    YRev is 8 - Y,
    
    % Scoring table
    score_pawn_table(ScoringTable),

    % Row
    nth0(YRev, ScoringTable, Row),

    % Score
    nth0(XRev, Row, Score).

score_piece(piece(_, pawn, _), 1).                % Default value

% Default
score_piece(piece(_, _, _), 0).


%! score_pawn_table(+ScoringTable)
%
%  Scoring table for scoring pawns (from the perspective of the white player)
%
%  Pawns will receive a higher score as they advance. 
%  This will prevent them from staying center, blocking other pieces to move forward.
%  Based on Hans Berliner's System.
score_pawn_table([
    [0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00],
    [0.90, 0.95, 1.05, 1.10, 1.10, 1.05, 0.95, 0.90],
    [0.90, 0.95, 1.05, 1.15, 1.15, 1.05, 0.95, 0.90],
    [0.90, 0.95, 1.10, 1.20, 1.20, 1.10, 0.95, 0.90],
    [0.97, 1.03, 1.17, 1.27, 1.27, 1.17, 1.03, 0.97],
    [1.06, 1.12, 1.25, 1.40, 1.40, 1.25, 1.12, 1.06]
]).