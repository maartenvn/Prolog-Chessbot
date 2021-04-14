:- module(util, []).

%! delete_list(+List, +ListToDelete, -ListNew)
%
%  Delete a list from another list.
delete_list(List, [Elem | Rest], ListNew) :-
    delete_list(List, Rest, ListNewRest),
    delete(ListNewRest, Elem, ListNew), !.
delete_list(List, [], List) :- !.