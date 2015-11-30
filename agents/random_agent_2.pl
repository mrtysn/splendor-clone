:- module(random_agent_2, []).

myName(random_agent_2).

:- dynamic tokens/2.
:- dynamic cards/2.
:- dynamic nobles/2.
:- dynamic reserves/2.
:- dynamic prestige/2.

init_agent(_agents, _area1, _area2, _area3, _nobles) :-
	proper_length(_agents, _nAgent),
	(	_nAgent = 2 -> _nToken = 4;
		_nAgent = 3 -> _nToken = 5;
		_nAgent = 4 -> _nToken = 7;
		false),
	update_tokens(board, 
		[_nToken, _nToken, _nToken, _nToken, _nToken, 5]),

	update_cards(area1, _area1),
	update_cards(area2, _area2),
	update_cards(area3, _area3),

	update_nobles(board, _nobles),

	%myName(_myName),
	forall(
		(
			between(1, _nAgent, _i),
			nth1(_i, _agents, _agent)
		),
		(
			assert(tokens(_agent, [0, 0, 0, 0, 0, 0])),
			assert(cards(_agent, [])),
			assert(nobles(_agent, [])),
			assert(reserves(_agent, [])),
			assert(prestige(_agent, 0))
		)
	).

ask_action(_cardsAffordable, _action) :-
	take_tokens(_tokens),
	append([[1], [_tokens]], _action).

take_tokens(_tokens) :-
	tokens(board, _tokensBoard),
	random_select(_r, [2, 3], _),
	(_r = 2 -> 
		(
			!,
			(take_2_tokens([1,2,3,4,5], _tokensBoard, [], _selection), X = 2);
			(take_3_tokens(3, [1,2,3,4,5], _tokensBoard, [], _selection), X = 3)	
		);
		(
			!,
			(take_3_tokens(3, [1,2,3,4,5], _tokensBoard, [], _selection), X = 3);
			(take_2_tokens([1,2,3,4,5], _tokensBoard, [], _selection), X = 2)
		)
	),
	generate_tokens(X, X, _selection, [0,0,0,0,0], _tokens).

take_3_tokens(_, [], _, List, List) :- !.
take_3_tokens(0, _, _, List, List) :- !.
take_3_tokens(Count, _possibilities, Board, List, ListNew) :-
	random_select(_r, _possibilities, _possibilitiesNew),
	nth1(_r, Board, _nToken),
	(_nToken > 0 -> 
		(
			!,
			append(List, [_r], ListTemp),
			CountNew is Count - 1,
			take_3_tokens(CountNew, _possibilitiesNew, Board, ListTemp, ListNew)
		);
		(
			!,
			take_3_tokens(Count, _possibilitiesNew, Board, List, ListNew)
		)
	).

take_2_tokens([], _, List, List) :- !.
take_2_tokens(_possibilities, Board, List, ListNew) :-
	random_select(_r, _possibilities, _possibilitiesNew),
	nth1(_r, Board, _nToken),
	(_nToken > 3 -> 
		(
			!,
			append(List, [_r], ListTemp),
			take_2_tokens([], Board, ListTemp, ListNew)
		);
		(
			!,
			take_2_tokens(_possibilitiesNew, Board, List, ListNew)
		)
	).

generate_tokens(2, _, _selection, _, _tokens) :-
	nth1(1, _selection, _i),
	nth1(_i, _tokens, 2, [0,0,0,0]).

generate_tokens(3, _, [], From, From) :- !.
generate_tokens(3, 0, _, From, From) :- !.
generate_tokens(3, Count, _selection, From, To) :-
	nth1(1, _selection, _i1, _selectionNew),
	nth1(_i1, From, _, FromTemp),
	nth1(_i1, FromNew, 1, FromTemp),
	CountNew is Count - 1,
	generate_tokens(3, CountNew, _selectionNew, FromNew, To).


announce_action(_agent, [1, _actionParams]) :-
	append(_actionParams, [0], _tokens),

	tokens(board, _tokensBoard),
	maplist(plus, _tokens, _tokensBoardNew, _tokensBoard),
	update_tokens(board, _tokensBoardNew),

	tokens(_agent, _tokensAgent),
	maplist(plus, _tokens, _tokensAgent, _tokensAgentNew),
	update_tokens(_agent, _tokensAgentNew).

announce_action(_agent, [_actionType, _actionParams]).

multiple_noble_visit(_noblesVisiting, _nobleAccepted) :-
	true
.

token_overload(_overloadedTokens, _returnedTokens) :-
	proper_length(_overloadedTokens, _tokenCount),
	_returnCount is _tokenCount - 10,
	return_tokens(_returnCount, _overloadedTokens, [0,0,0,0,0,0], _, _returnedTokens), 
	!.

return_tokens(_, [], To, [], To) :- !.
return_tokens(0, From, To, From, To) :- !.
return_tokens(N, From, To, Rest, Result) :-
	N > 0, M is N-1,
	random_between(1, 6, Index),
	nth1(Index, From, ToBeDecremented, AccFrom),
	(ToBeDecremented > 0 ->
		(
			Decremented is ToBeDecremented - 1,
			nth1(Index, FromNew, Decremented, AccFrom),

			nth1(Index, To, ToBeIncremented, AccTo),
			Incremented is ToBeIncremented + 1,
			nth1(Index,  ToNew, Incremented, AccTo),

			return_tokens(M, FromNew, ToNew, Rest, Result)

		);
		return_tokens(M, From, To, Rest, Result)
	).


update_tokens(_owner, _tokens) :-
	(retract(tokens(_owner, _));true),
	assert(tokens(_owner, _tokens)),
	!.

update_cards(_owner, _cards) :-
	(retract(cards(_owner, _));true),
	assert(cards(_owner, _cards)),
	!.

update_nobles(_owner, _nobles) :-
	(retract(nobles(_owner, _));true),
	assert(nobles(_owner, _nobles)),
	!.

update_reserves(_owner, _reserves) :-
	(retract(reserves(_owner, _));true),
	assert(reserves(_owner, _reserves)),
	!.

update_prestige(_owner, _prestige) :-
	(retract(prestige(_owner, _));true),
	assert(prestige(_owner, _prestige)),
	!.