action(_agent) :-
	get_affordable_cards(_agent, _cardsAffordable),
	!,
	ask_action(_agent, _cardsAffordable, _action),
	!,
	verify_action(_agent, _cardsAffordable, _action),
	!,
	apply_action(_agent, _action),
	!,
	announce_action(_agent, _action),
	!.

ask_action(_agent, _cardsAffordable, _action) :-
	_agent:ask_action(_cardsAffordable, _action).

verify_action(_agent, _cardsAffordable, [_actionType, _actionParams]) :-
	between(1, 3, _actionType),
	_actionType = 1 -> verify_take_tokens(_agent, _actionParams);
	_actionType = 2 -> verify_purchase_card(_agent, _cardsAffordable, _actionParams);
	_actionType = 3 -> verify_reserve_card(_agent, _actionParams);
	false.

apply_action(_agent, [_actionType, _actionParams]) :-
	between(1, 3, _actionType),
	_actionType = 1 -> take_tokens(_agent, _actionParams);
	_actionType = 2 -> purchase_card(_agent, _actionParams);
	_actionType = 3 -> reserve_card(_agent, _actionParams);
	false.

announce_action(_agent, _action) :-
	agents(_agents), proper_length(_agents, _nAgent),
	forall(
		(
			between(1, _nAgent, _i),
			nth1(_i, _agents, _otherAgent)
		),
		(
			_otherAgent:announce_action(_agent, _action)
		)
	)
.

verify_take_tokens(_, _tokens) :-
	proper_length(_tokens, 5),
	min_member(_min, _tokens), _min = 0,

	nth1(1, _tokens, _white),
	nth1(2, _tokens, _blue),
	nth1(3, _tokens, _green),
	nth1(4, _tokens, _red),
	nth1(5, _tokens, _black),

	tokens(board, _tokensBoard),
	nth1(1, _tokensBoard, _tokensBoardWhite),
	nth1(2, _tokensBoard, _tokensBoardBlue),
	nth1(3, _tokensBoard, _tokensBoardGreen),
	nth1(4, _tokensBoard, _tokensBoardRed),
	nth1(5, _tokensBoard, _tokensBoardBlack),

	_whiteLeft	is _tokensBoardWhite	- _white,
	_blueLeft	is _tokensBoardBlue		- _blue,
	_greenLeft	is _tokensBoardGreen	- _green,
	_redLeft	is _tokensBoardRed		- _red,
	_blackLeft	is _tokensBoardBlack	- _black,

	_whiteLeft	>= 0,
	_blueLeft	>= 0,
	_greenLeft	>= 0,
	_redLeft	>= 0,
	_blackLeft	>= 0,
	!,
	(_white > 0 -> _betaWhite	= 1;	_betaWhite	= 0),
	(_blue  > 0 -> _betaBlue	= 1;	_betaBlue	= 0),
	(_green > 0 -> _betaGreen	= 1;	_betaGreen	= 0),
	(_red 	> 0 -> _betaRed		= 1;	_betaRed	= 0),
	(_black > 0 -> _betaBlack	= 1;	_betaBlack	= 0),

	sum_list(_tokens, _tokensSum),
	_betaSum is (_betaWhite + _betaBlue + _betaGreen + _betaRed + _betaBlack),
	!,
	(
		(_betaSum = 1, _tokensSum = 2) -> 
			(
				!,
				(_betaWhite	= 0; _whiteLeft	>= 2),
				(_betaBlue	= 0; _blueLeft 	>= 2),
				(_betaGreen	= 0; _greenLeft >= 2),
				(_betaRed	= 0; _redLeft	>= 2),
				(_betaBlack = 0; _blackLeft >= 2),
				!
			);
			(
				(_betaSum = 3, _tokensSum = 3) -> 
					!;
					(
						(_tokensSum = 2; _tokensSum = 1) -> 
							(
								!,
								(_white = 0 -> _whiteLeft	= 0; true),
								(_blue 	= 0 -> _blueLeft	= 0; true),
								(_green = 0 -> _greenLeft	= 0; true),
								(_red 	= 0 -> _redLeft		= 0; true),
								(_black = 0 -> _blackLeft	= 0; true),
								!
							);
							(
								!, false
							)
					)
			)
	),
	!.
take_tokens(_agent, _actionParams) :-
	write(_agent), write(' '), write(_actionParams), nl,
	append(_actionParams, [0], _tokens),
	
	tokens(board, _tokensBoard),
	maplist(plus, _tokens, _tokensBoardNew, _tokensBoard),
	
	tokens(_agent, _tokensAgent),
	maplist(plus, _tokens, _tokensAgent, _tokensAgentNew),

	update_tokens(board, _tokensBoardNew),
	update_tokens(_agent, _tokensAgentNew),
	!.

verify_purchase_card(_agent, _cardsAffordable, [_tier, _position, _tokens]) :-
	between(1, 3, _tier),
	between(1, 4, _position),

	member([_tier, _position], _cardsAffordable),

	atom_concat(area, _tier, _area),
	cards(_area, _cardsArea),

	nth1(_position, _cardsArea, _card),
	append([_,_,_], _cardCostTemp, _card),
	append(_cardCostTemp, [0], _cardCost),

	cards(_agent, _cardsAgent),
	calculate_agent_card_wealth(_cardsAgent, _cardWealthAgent),

	maplist(plus, _cardWealthAgent, _cardEffectiveCostTemp, _cardCost),
	negative_to_zero(_cardEffectiveCostTemp, _cardEffectiveCost),
	maplist(plus, _tokens, _surplus, _cardEffectiveCost),
	negative_to_zero(_surplus, _surplusActual),
	sum_list(_surplusActual, _yellowTokensNecessary),
	nth1(6, _tokens, _yellowTokens),
	_yellowTokensNecessary = _yellowTokens,
	!.
purchase_card(_agent, [0, _position, _tokens]) :-
	tokens(_agent, _tokensAgent),
	maplist(plus, _tokens, _tokensAgentNew, _tokensAgent),

	tokens(board, _tokensBoard),
	maplist(plus, _tokens, _tokensBoard, _tokensBoardNew),

	reserves(_agent, _reservesAgent),
	nth1(_position, _reservesAgent, _card),

	delete(_reservesAgent, _card, _reservesAgentNew),

	cards(_agent, _cardsAgent),
	append(_cardsAgent, [_card], _cardsAgentNew),

	nth1(2, _card, _prestigeCard),
	prestige(_agent, _prestigeAgent),
	_prestigeAgentNew is _prestigeAgent + _prestigeCard,

	update_tokens(_agent, _tokensAgentNew),
	update_tokens(board, _tokensBoardNew),
	update_cards(_agent, _cardsAgentNew),
	update_reserves(_agent, _reservesAgentNew),
	update_prestige(_agent, _prestigeAgentNew),
	!.
purchase_card(_agent, [_tier, _position, _tokens]) :-
	tokens(_agent, _tokensAgent),
	maplist(plus, _tokens, _tokensAgentNew, _tokensAgent),

	tokens(board, _tokensBoard),
	maplist(plus, _tokens, _tokensBoard, _tokensBoardNew),

	atom_concat(area, _tier, _area),
	cards(_area, _cardsArea),
	nth1(_position, _cardsArea, _card),

	cards(_agent, _cardsAgent),
	append(_cardsAgent, [_card], _cardsAgentNew),

	atom_concat(deck, _tier, _deck),
	cards(_deck, _cardsDeck),

	select(_card, _cardsArea, null, _cardsAreaTemp),
	move_n_random(1, _cardsDeck, _cardsAreaTemp,
		_cardsDeckNew, _cardsAreaNew),

	nth1(2, _card, _prestigeCard),
	prestige(_agent, _prestigeAgent),
	_prestigeAgentNew is _prestigeAgent + _prestigeCard,

	update_tokens(_agent, _tokensAgentNew),
	update_tokens(board, _tokensBoardNew),
	update_cards(_agent, _cardsAgentNew),
	update_cards(_area, _cardsAreaNew),
	update_cards(_deck, _cardsDeckNew),
	update_prestige(_agent, _prestigeAgentNew),
	!.

verify_reserve_card(_agent, [_tier, _position]) :-
	reserves(_agent, _reservesAgent),
	proper_length(_reservesAgent, _reservesAgentCount),
	_reservesAgentCount < 3,

	between(1, 3, _tier),

	(
		(_position = 0) -> 
			(
				atom_concat(deck, _tier, _deck),
				cards(_deck, _cardsDeck),
				\+ _cardsDeck = []
			);
			(
				atom_concat(area, _tier, _area),
				cards(_area, _cardsArea),
				between(1, 4, _position),
				nth1(_position, _cardsArea, _card),
				\+ _card = null
			)
	),
	!.
reserve_card(_agent, [_tier, _position]) :-
	reserves(_agent, _reservesAgent),

	atom_concat(deck, _tier, _deck),
	cards(_deck, _cardsDeck),
	(
		(_position = 0) -> 
			(	
				append(_reservesAgent, [null], _reservesAgentTemp),
				move_n_random(1, _cardsDeck, _reservesAgentTemp,
					_cardsDeckNew, _reservesAgentNew)
			);
			(
				atom_concat(area, _tier, _area),
				cards(_area, _cardsArea),
				nth1(_position, _cardsArea, _card),
				append(_reservesAgent, [_card], _reservesAgentNew),
				select(_card, _cardsArea, null, _cardsAreaTemp),
				move_n_random(1, _cardsDeck, _cardsAreaTemp,
					_cardsDeckNew, _cardsAreaNew),
				update_cards(_area, _cardsAreaNew)
			)
	),
	update_cards(_deck, _cardsDeckNew),
	update_reserves(_agent, _reservesAgentNew),

	tokens(board, _tokensBoard),
	nth1(6, _tokensBoard, _tokensBoardYellow),
	(
		(_tokensBoardYellow > 0) -> 
			(	
				_tokensBoardYellowNew is _tokensBoardYellow - 1,
				append(_tokensBoardTemp, [_], _tokensBoard),
				append(_tokensBoardTemp, [_tokensBoardYellowNew], _tokensBoardNew),

				tokens(_agent, _tokensAgent),
				nth1(6, _tokensAgent, _tokensAgentYellow),
				_tokensAgentYellowNew is _tokensAgentYellow + 1,
				append(_tokensAgentTemp, [_], _tokensAgent),
				append(_tokensAgentTemp, [_tokensAgentYellowNew], _tokensAgentNew),

				update_tokens(board, _tokensBoardNew),
				update_tokens(_agent, _tokensAgentNew)
			);
			(
				true
			)
	),
	!.

token_overload_check(_agent) :-
	tokens(_agent, _tokensAgent),
	proper_length(_tokensAgent, _nToken),
	(	
		_nToken > 10 ->
			(
				(
					(
						_agent:token_overload(_tokensAgent, _tokensReturned),
						maplist(plus, _tokensAgentNew, _tokensReturned, _tokensAgent),
						proper_length(_tokensAgentNew, _nTokenNew),
						_nTokenNew = 10
					);
					(
						proper_length(_tokensAgent, _tokenCount),
						_returnCount is _tokenCount - 10,
						random_tokens(_returnCount, _tokensAgent, [0,0,0,0,0,0], _tokensAgentNew, _tokensReturned)
					)
				),
				
				tokens(board, _tokensBoard),
				maplist(plus, _tokensBoard, _tokensReturned, _tokensBoardNew),

				update_tokens(_agent, _tokensAgentNew),
				update_tokens(board, _tokensBoardNew)
			);
			(
				true		
			)
	), !.

random_tokens(_, [], To, [], To) :- !.
random_tokens(0, From, To, From, To) :- !.
random_tokens(N, From, To, Rest, Result) :-
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

			random_tokens(M, FromNew, ToNew, Rest, Result)

		);
		random_tokens(M, From, To, Rest, Result)
	).

noble_check(_agent) :-
	get_affordable_nobles(_agent, _noblesAffordable),
	(	
		_noblesAffordable = [] ->
			(
				true
			);
			(
				(
					(_agent:choose_noble(_noblesAffordable, _noble),
						member(_noble, _noblesAffordable));	
					(random_select(_noble, _noblesAffordable, _))
				),

				prestige(_agent, _prestigeAgent),
				_prestigeAgentNew is _prestigeAgent + 3,
				update_prestige(_agent, _prestigeAgentNew),
		
				nobles(board, _noblesBoard),
				select(_noble, _noblesBoard, null, _noblesBoardNew),
				update_nobles(board, _noblesBoardNew),
				
				nobles(_agent, _noblesAgent),
				append(_noblesAgent, [_noble], _noblesAgentNew),
				update_nobles(_agent, _noblesAgentNew)
			)
	), !.

% random action

% random return token

% random pick noble