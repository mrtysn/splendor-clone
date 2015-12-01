:- module(human, []).

init_agent(_, _, _, _, _).

ask_action(_cardsAffordable, _action) :-
	write('What is your action type?'), nl,
	write('Type: '), read(_actionType),

	(_actionType = 2 -> (
		write('Cards affordable:'), nl,
		write(_cardsAffordable), nl
		);(
		true
		)),
	write('What are your action parameters?'), nl,
	write('Parameters: '), read(_actionParameters),

	append([_actionType], [_actionParameters], _action).

announce_action(_, _).

token_overload(_overloadedTokens, _returnedTokens) :-
	write('Overloaded tokens: '), nl,
	write(_overloadedTokens), nl,
	write('Which tokens will you return? '),
	read(_returnedTokens).

choose_noble(_noblesAffordable, _noble) :-
	write('Nobles visiting you: '), nl,
	write(_noblesAffordable), nl,
	write('Which noble do you accept? '),
	read(_i),
	nth1(_i, _noblesAffordable, _noble).