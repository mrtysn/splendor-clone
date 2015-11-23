:- use_module(library(pce)).
:- use_module(library(scaledbitmap)).

card_edge(200).
noble_edge(120).
token_edge(80).

test :-
	draw_tokens,

	update_card(501, 0, 1),
	update_card(502, 0, 2),
	update_card(503, 0, 3),
	update_card(504, 0, 4),
	update_card(505, 0, 5),

	update_card(201, 3, 1),
	update_card(202, 3, 2),
	update_card(203, 3, 3),
	update_card(204, 3, 4),

	update_card(101, 2, 1),
	update_card(102, 2, 2),
	update_card(103, 2, 3),
	update_card(104, 2, 4),

	update_card(1, 1, 1),
	update_card(2, 1, 2),
	update_card(3, 1, 3),
	update_card(4, 1, 4),

	!.

draw_tokens :-
	token_edge(_tokenEdge),
	foreach((between(1,6, _index), 
		atomic_list_concat(['./resources/token', _index, '.jpg'], _path),
		atomic_list_concat(['token', _index], _reference),
		new(_card, scaled_bitmap(image(_path)))
		), 
		(
		send(_card, scale, size(_tokenEdge, _tokenEdge)),
		send(@_reference, append, _card), !)
		),
	!
	.

update_card(_id, _tier, _position) :-
	card_edge(_cardEdge), noble_edge(_nobleEdge),

	atomic_list_concat(['./resources/', _id, '.jpg'], _path),
	atomic_list_concat(['tier', _tier, 'slot', _position], _reference),
	
	((get(@_reference, member(bitmap), _currentCard), free(_currentCard));true),
	new(_card, scaled_bitmap(image(_path))),
	(_tier = 0 -> 
		send(_card, scale, size(_nobleEdge, _nobleEdge));
		send(_card, scale, size(_cardEdge, _cardEdge))
	),
	
	send(@_reference, append, _card).

% USE pce_global
free_handles() :-
	(free(@tier3slot0);true),
	(free(@tier3slot1);true),
	(free(@tier3slot2);true),
	(free(@tier3slot3);true),
	(free(@tier3slot4);true),
	(free(@tier2slot0);true),
	(free(@tier2slot1);true),
	(free(@tier2slot2);true),
	(free(@tier2slot3);true),
	(free(@tier2slot4);true),
	(free(@tier1slot0);true),
	(free(@tier1slot1);true),
	(free(@tier1slot2);true),
	(free(@tier1slot3);true),
	(free(@tier1slot4);true),
	(free(@tier0slot1);true),
	(free(@tier0slot2);true),
	(free(@tier0slot3);true),
	(free(@tier0slot4);true),
	(free(@tier0slot5);true),
	(free(@token1);true),
	(free(@token2);true),
	(free(@token3);true),
	(free(@token4);true),
	(free(@token5);true),
	(free(@token6);true),
	(free(@board);true).

draw :-
	free_handles(),
	new(@board, dialog('Splendor')),
	create_scoreboard,
	create_card_area,
	create_token_area,
	create_noble_area,
	create_focus_area,
	send(@board, open),
	!.

create_scoreboard :-
	_nullTokens = '0 : 0 / 0 : 0 / 0 : 0 / 0 : 0 / 0 : 0 / 0', % atomic_list_concat([0,0,0,0,0,0], ' / ', X)
	send(@board, append, new(_scoreBoard, dialog_group('Score Board', group))),
	
	send(_scoreBoard, append, new(_agent1Area, dialog_group('Agent 1 Name', box))),
	send(_agent1Area, append, new(_agent1Score, label(text, 'Score')), next_row),
	send(_agent1Area, append, new(_agent1Chips, label(text, 'Chips: 0/10')), next_row),
	send(_agent1Area, append, new(_agent1Tokens, label(text, _nullTokens)), next_row),
	send(_agent1Area, append, new(_agent1Reserves, label(text, 'Reserves')), next_row),
	
	send(_scoreBoard, append, new(_agent2Area, dialog_group('Agent 2 Name', box))),
	send(_agent2Area, append, new(_agent2Score, label(text, 'Score')), next_row),
	send(_agent2Area, append, new(_agent2Chips, label(text, 'Chips: 0/10')), next_row),
	send(_agent2Area, append, new(_agent2Tokens, label(text, _nullTokens)), next_row),
	send(_agent2Area, append, new(_agent2Reserves, label(text, 'Reserves')), next_row),
	
	send(_scoreBoard, append, new(_agent3Area, dialog_group('Agent 3 Name', box))),
	send(_agent3Area, append, new(_agent3Score, label(text, 'Score')), next_row),
	send(_agent3Area, append, new(_agent3Chips, label(text, 'Chips: 0/10')), next_row),
	send(_agent3Area, append, new(_agent3Tokens, label(text, _nullTokens)), next_row),
	send(_agent3Area, append, new(_agent3Reserves, label(text, 'Reserves')), next_row),
	
	send(_scoreBoard, append, new(_agent4Area, dialog_group('Agent 4 Name', box))),
	send(_agent4Area, append, new(_agent4Score, label(text, 'Score')), next_row),
	send(_agent4Area, append, new(_agent4Chips, label(text, 'Chips: 0/10')), next_row),
	send(_agent4Area, append, new(_agent4Tokens, label(text, _nullTokens)), next_row),
	send(_agent4Area, append, new(_agent4Reserves, label(text, 'Reserves')), next_row).

create_card_area :-
	card_edge(_cardEdge),
	send(@board, append, new(_decks, dialog_group('cards', group)), right),

	send(_decks, append, new(_tier3, dialog_group('Tier 3', box))),
	send(_decks, append, new(_tier2, dialog_group('Tier 2', box)), next_row),
	send(_decks, append, new(_tier1, dialog_group('Tier 1', box)), next_row),

	_cardSize = size(_cardEdge, _cardEdge),
	_gapSize = size(_cardEdge / 10, _cardEdge / 10),
	_tierSize = size(_cardEdge * 56 / 10, _cardEdge * 12 / 10),
	
	send(_tier3, gap, _gapSize),
	send(_tier3, size, _tierSize),
	send(_tier3, append, new(@tier3slot0, dialog(size, _cardSize))),
	send(_tier3, append, new(@tier3slot1, dialog(size, _cardSize)), right),
	send(_tier3, append, new(@tier3slot2, dialog(size, _cardSize)), right),
	send(_tier3, append, new(@tier3slot3, dialog(size, _cardSize)), right),
	send(_tier3, append, new(@tier3slot4, dialog(size, _cardSize)), right),

	send(_tier2, gap, _gapSize),
	send(_tier2, size, _tierSize),
	send(_tier2, append, new(@tier2slot0, dialog(size, _cardSize))),
	send(_tier2, append, new(@tier2slot1, dialog(size, _cardSize)), right),
	send(_tier2, append, new(@tier2slot2, dialog(size, _cardSize)), right),
	send(_tier2, append, new(@tier2slot3, dialog(size, _cardSize)), right),
	send(_tier2, append, new(@tier2slot4, dialog(size, _cardSize)), right),

	send(_tier1, gap, _gapSize),
	send(_tier1, size, _tierSize),
	send(_tier1, append, new(@tier1slot0, dialog(size, _cardSize))),
	send(_tier1, append, new(@tier1slot1, dialog(size, _cardSize)), right),
	send(_tier1, append, new(@tier1slot2, dialog(size, _cardSize)), right),
	send(_tier1, append, new(@tier1slot3, dialog(size, _cardSize)), right),
	send(_tier1, append, new(@tier1slot4, dialog(size, _cardSize)), right),

		new(_tier3slot0, scaled_bitmap(image('./resources/tier3.jpg'))),
	new(_tier2slot0, scaled_bitmap(image('./resources/tier2.jpg'))),
	new(_tier1slot0, scaled_bitmap(image('./resources/tier1.jpg'))),
	send(_tier3slot0, scale, _cardSize),
	send(_tier2slot0, scale, _cardSize),
	send(_tier1slot0, scale, _cardSize),
	send(@tier3slot0, append, _tier3slot0),
	send(@tier2slot0, append, _tier2slot0),
	send(@tier1slot0, append, _tier1slot0),

	!.

create_token_area :-
	token_edge(_tokenEdge),

	send(@board, append, new(_tokens, dialog_group('Tokens', box)), right),
	send(_tokens, gap, size(_tokenEdge / 10, _tokenEdge / 10)),
	send(_tokens, size, size(_tokenEdge * 12 / 10, _tokenEdge * 67 / 10)),
	_tokenSize = size(_tokenEdge, _tokenEdge),

	send(_tokens, append, new(@token1, dialog(size, _tokenSize)), next_row),
	send(_tokens, append, new(@token2, dialog(size, _tokenSize)), next_row),
	send(_tokens, append, new(@token3, dialog(size, _tokenSize)), next_row),
	send(_tokens, append, new(@token4, dialog(size, _tokenSize)), next_row),
	send(_tokens, append, new(@token5, dialog(size, _tokenSize)), next_row),
	send(_tokens, append, new(@token6, dialog(size, _tokenSize)), next_row).

create_noble_area :-
	noble_edge(_nobleEdge),
	send(@board, append, new(_nobles, dialog_group('Nobles', box)), right),

	send(_nobles, gap, size(_nobleEdge / 10, _nobleEdge / 10)),
	% MAKE THIS VARY WITH NOBLE SIZE, SO: CHANGE 5 TO #NOBLE, 6 TO #NOBLE+1
	send(_nobles, size, size(_nobleEdge * 12 / 10, _nobleEdge * 56 / 10)),
	_nobleSize = size(_nobleEdge, _nobleEdge),
	
	send(_nobles, append, new(@tier0slot1, dialog(size, _nobleSize)), next_row),
	send(_nobles, append, new(@tier0slot2, dialog(size, _nobleSize)), next_row),
	send(_nobles, append, new(@tier0slot3, dialog(size, _nobleSize)), next_row),
	send(_nobles, append, new(@tier0slot4, dialog(size, _nobleSize)), next_row),
	send(_nobles, append, new(@tier0slot5, dialog(size, _nobleSize)), next_row).

create_focus_area :-
	send(@board, append, new(_currentAgent, dialog_group('Current Agent', box)), next_row),
	
	_nullTokens = '0 : 0 / 0 : 0 / 0 : 0 / 0 : 0 / 0 : 0 / 0', % atomic_list_concat([0,0,0,0,0,0], ' / ', X)
	send(_currentAgent, append, new(_currentAgentScore, label(text, 'Score'))),
	send(_currentAgent, append, new(_currentAgentChips, label(text, 'Chips: 0/10')), right),
	send(_currentAgent, append, new(_currentAgentTokens, label(text, _nullTokens)), right),
	send(_currentAgent, append, new(_currentAgentReserves, label(text, 'Reserves')), right).