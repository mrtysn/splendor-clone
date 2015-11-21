
:- dynamic tokens/2.
:- dynamic cards/2.
:- dynamic nobles/2.
:- dynamic reserves/2.

:- include(deck).

agents([bot1, bot2, bot3, bot4]). % Read from file

initialization() :-
	agents(_agents), proper_length(_agents, _nAgent),
	(	_nAgent == 2 -> _nToken = 4;
		_nAgent == 3 -> _nToken = 5;
		_nAgent == 4 -> _nToken = 7;
		false),
	_nNoble is _nAgent + 1,

	cards(deck1, _deck1),
	cards(deck2, _deck2),
	cards(deck3, _deck3),
	nobles(board, _nobles),

	list_n_null(4, _null4),
	list_n_null(_nNoble, _nullNobles),

	draw_n_cards(4, _deck1, _null4, _deck1New, _area1), !,
	draw_n_cards(4, _deck2, _null4, _deck2New, _area2), !,
	draw_n_cards(4, _deck3, _null4, _deck3New, _area3), !,
	draw_n_cards(_nNoble, _nobles, _nullNobles, _, _noblesNew), !,

	% Reset DB?
	
	(retract(cards(deck1, _)); true),
	(retract(cards(deck2, _)); true),
	(retract(cards(deck3, _)); true),
	(retract(cards(area1, _)); true),
	(retract(cards(area2, _)); true),
	(retract(cards(area3, _)); true),
	(retract(nobles(board, _)); true),
	(retract(tokens(board, _)); true),

	assert(cards(deck1, _deck1New)), !,
	assert(cards(deck2, _deck2New)), !,
	assert(cards(deck3, _deck3New)), !,
	assert(cards(area1, _area1)), !,
	assert(cards(area2, _area2)), !,
	assert(cards(area3, _area3)), !,
	assert(nobles(board, _noblesNew)), !,
	assert(tokens(board, [_nToken, _nToken, _nToken, _nToken, _nToken, 3])), !,
	forall(between(1, _nAgent, _n),
		(
			atom_concat(agent, _n, _agent),
			assert(tokens(_agent, [])), !,
			assert(cards(_agent, [])), !,
			assert(reserves(_agent, [])), !,

			% nth1(PlayerNo, PlayerModules, PlayerModule)
			% atom_concat('players/', PlayerModule, PlayerModuleFile)
			% use_module(PlayerModuleFile),
			% PlayerModule:initialize(Player, PlayerCount)
			true % remove this line
		)
	),


	true. % remove this line


%	set_tokens(),
%	set_decks(),
%	draw_cards_from_deck_to_area(),
%	draw_nobles(),
%	set_player_tokens(),
%	set_player_cards().


draw_n_cards(_, [], To, [], To) :- !.
draw_n_cards(0, From, To, From, To) :- !.
draw_n_cards(N, From, To, Rest, Result) :-
	N > 0, M is N-1,
	(random_select(Card, From, Remainder) -> 
		(select(null, To, Card, Acc), draw_n_cards(M, Remainder, Acc, Rest, Result));
		true). % check if deck is empty, add true and do nothing?

list_n_null(0, []) :- !.
list_n_null(1, [null]) :- !.
list_n_null(N, [null|List]) :-
	N > 0, M is N-1,
	list_n_null(M, List).

list_n_duplicate(0, _, []) :- !.
list_n_duplicate(1, X, [X]) :- !.
list_n_duplicate(N, X, [X|List]) :-
	N > 0, M is N-1,
	list_n_duplicate(M, X, List).


take_tokens(_agent, _tokens) :-
	proper_length(_tokens, 5), min_member(_min, _tokens), _min = 0,
	nth1(1, _tokens, _white),
	nth1(2, _tokens, _blue),
	nth1(3, _tokens, _green),
	nth1(4, _tokens, _red),
	nth1(5, _tokens, _black),

	tokens(board, _tokensBoard),
	nth1(1, _tokensBoard, _whiteBoard),
	nth1(2, _tokensBoard, _blueBoard),
	nth1(3, _tokensBoard, _greenBoard),
	nth1(4, _tokensBoard, _redBoard),
	nth1(5, _tokensBoard, _blackBoard),
	nth1(6, _tokensBoard, _yellowBoard),

	_whiteLeft is _whiteBoard - _white,
	_blueLeft is _blueBoard - _blue,
	_greenLeft is _greenBoard - _green,
	_redLeft is _redBoard - _red,
	_blackLeft is _blackBoard - _black,

	_whiteLeft >= 0,
	_blueLeft >= 0,
	_greenLeft >= 0,
	_redLeft >= 0,
	_blackLeft >= 0,

	(_white > 0 -> _betaWhite = 1; _betaWhite = 0),
	(_blue > 0 -> _betaBlue = 1; _betaBlue = 0),
	(_green > 0 -> _betaGreen = 1; _betaGreen = 0),
	(_red > 0 -> _betaRed = 1; _betaRed = 0),
	(_black > 0 -> _betaBlack = 1; _betaBlack = 0),

	sum_list(_tokens, _tokensSum),
	_betaSum is (_betaWhite + _betaBlue + _betaGreen + _betaRed + _betaBlack),

	(
		(_tokensSum = 2, _betaSum = 1) -> 
			(
				(_betaWhite = 0; _whiteLeft >= 2),
				(_betaBlue = 0; _blueLeft >= 2),
				(_betaGreen = 0; _greenLeft >= 2),
				(_betaRed = 0; _redLeft >= 2),
				(_betaBlack = 0; _blackLeft >= 2)
			);
		(_tokensSum = 3, _betaSum = 3) -> !;
		((_tokensSum = 2, _betaSum = 2); _tokensSum = 1) -> 
			(
				(_white = 0 -> _whiteLeft = 0; _whiteLeft < 3),
				(_blue = 0 -> _blueLeft = 0; _blueLeft < 3),
				(_green = 0 -> _greenLeft = 0; _greenLeft < 3),
				(_red = 0 -> _redLeft = 0; _redLeft < 3),
				(_black = 0 -> _blackLeft = 0; _blackLeft < 3)
			);
		false
	), !,

	retract(tokens(board, _)),
	assert(tokens(board, [_whiteLeft, _blueLeft, _greenLeft, _redLeft, _blackLeft, _yellowBoard])),

	tokens(_agent, _tokensAgent),
	maplist(plus, _tokensAgent, _tokens, _tokensAgentNew),
	retract(tokens(_agent, _)),
	assert(tokens(_agent, _tokensAgentNew)).

purchase_card(_agent, _tier, _position) :- % DO NOT FORGET ABOUT YELLOW TOKENS
	between(1, 3, _tier),
	between(1, 4, _position),

	atom_concat(area, _tier, _area),
	atom_concat(deck, _tier, _deck),
	
	cards(_area, _cardsArea),
	cards(_deck, _cardsDeck),
	cards(_agent, _cardsAgent),
	tokens(_agent, _tokensAgent),

	nth1(_position, _cardsArea, _card),
	append([_,_,_], _cardCost, _card),
	
	maplist(nth1(3), _cardsAgent, _cardsAgentColors),
	aggregate_all(count, member('white', _cardsAgentColors), _whiteCards),
	aggregate_all(count, member('blue', _cardsAgentColors), _blueCards),
	aggregate_all(count, member('green', _cardsAgentColors), _greenCards),
	aggregate_all(count, member('red', _cardsAgentColors), _redCards),
	aggregate_all(count, member('black', _cardsAgentColors), _blackCards),
	_developmentsAgent = [_whiteCards, _blueCards, _greenCards, _redCards, _blackCards],
		
	maplist(plus, _tokensAgent, _developmentsAgent, _wealthAgent),
	maplist(plus, _cardCost, _leftoverAgent, _wealthAgent),
	maplist(between(0, infinite), _leftoverAgent), % affordance check

	maplist(plus, _developmentsAgent, _surplusTemp, _cardCost),
	negative_to_zero(_surplusTemp, _surplus),
	maplist(plus, _tokensAgentNew, _surplus, _tokensAgent),
	append(_cardsAgent, _card, _cardsAgentNew),
	
	retract(cards(_agent, _)),
	assert(cards(_agent, _cardsAgentNew)),
	retract(tokens(_agent, _)),
	assert(tokens(_agent, _tokensAgentNew)),

	select(_card, _cardsArea, null, _cardsAreaTemp),
	draw_n_cards(1, _cardsDeck, _cardsAreaTemp, _cardsDeckNew, _cardsAreaNew),

	retract(cards(_area, _)),
	assert(cards(_area, _cardsAreaNew)),
	retract(cards(_deck, _)),
	assert(cards(_deck, _cardsDeckNew)),

	true. % delete this line



negative_to_zero([], []) :- !.
negative_to_zero([H], [0]) :-
	H < 0, !.
negative_to_zero([H], [H]) :- !.
negative_to_zero([H|T], [0|Result]) :-
	H < 0, !,
	negative_to_zero(T, Result).
negative_to_zero([H|T], [H|Result]) :-
	negative_to_zero(T, Result).