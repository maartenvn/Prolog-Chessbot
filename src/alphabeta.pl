:- use_module("moves").


%! alphabeta(+CurrentBoard, +CurrentColor, +Depth, +LowerBound, +UpperBound, -BestMove, -BestScore)
%
alphabeta(CurrentBoard, CurrentColor, Depth, LowerBound, UpperBound, BestMove, BestScore) :-
    State = alphabeta_state(Depth, LowerBound, UpperBound),

    % Depth must be larger than 0
    Depth > 0,

    % Decrement the depth
    NextDepth is Depth - 1,

    % Determin all possible moves for the current color
    moves:all_possible_moves(CurrentColor, CurrentBoard, NextMoves),

    % Determin all possible boards for all the possible moves
    moves:all_possible_boards(NextMoves, CurrentBoard, NextBoards).

    % Determin the best move for the list of moves

%! best(+NextBoards, +NextColor, +NextDepth, +LowerBound, +UpperBound, -BestMove, -BestScore)
best(NextBoards, NextColor, NextDepth, LowerBound, UpperBound, BestMove, BestScore).