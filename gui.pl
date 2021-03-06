:- use_module(library(scaledbitmap)).
:- use_module(library(tabular)).

card_edge(200).
noble_edge(160).
token_edge(80).
reserve_edge(70).
area_size(size(210, 100)).
font_size(72).


draw_tokens :-
	token_edge(_tokenEdge),
	foreach((between(1, 6, _index), 
		atomic_list_concat(['./resources/tokens/token', _index, '.jpg'], _path),
		atomic_list_concat(['token', _index], _reference),
		new(_card, scaled_bitmap(image(_path)))
		), 
		(
		send(_card, scale, size(_tokenEdge, _tokenEdge)),
		send(@_reference, append, _card), !)
		),
	!.

mark_card([_tier, _position]) :-
	atomic_list_concat(['tier', _tier, 'slot', _position], _reference),
	get(@_reference, member, scaled_bitmap, _card),
	send_list(_card, [pen(5), colour(green), cursor(exchange)]),
	!.

unmark_card([_tier, _position]) :-
	atomic_list_concat(['tier', _tier, 'slot', _position], _reference),
	get(@_reference, member, scaled_bitmap, _card),
	send_list(_card, [pen(0), cursor('X_cursor')]),
	!.

mark_reserve(_agentId, _position) :-
	atomic_list_concat(['agent', _agentId, 'reserve', _position], _reference),
	get(@_reference, member, scaled_bitmap, _card),
	send_list(_card, [pen(5), colour(green), cursor(exchange)]),
	!.

unmark_reserve(_agentId, _position) :-
	atomic_list_concat(['agent', _agentId, 'reserve', _position], _reference),
	((get(@_reference, member, scaled_bitmap, _card),
	send_list(_card, [pen(0), cursor('X_cursor')]));true),
	!.

gui_update_card(_id, _tier, _position) :-
	card_edge(_cardEdge), noble_edge(_nobleEdge),

	atomic_list_concat(['./resources/cards/', _id, '.jpg'], _path),
	atomic_list_concat(['tier', _tier, 'slot', _position], _reference),
	
	((get(@_reference, member, scaled_bitmap, _currentCard), free(_currentCard));true),
	new(_card, scaled_bitmap(image(_path))),
	(_tier = 0 -> 
		send(_card, scale, size(_nobleEdge, _nobleEdge));
		send(_card, scale, size(_cardEdge, _cardEdge))
	),
	
	send(@_reference, append, _card).

gui_update_reserves(_agentId, _reserves):-
	reserve_edge(_reserveEdge),
	foreach(
		(
			between(1, 3, _index),
			atomic_list_concat(['agent', _agentId, 'reserve', _index], _reserveSlot),
			nth1(_index, _reserves, _card),
			nth1(1, _card, _id),
			atomic_list_concat(['./resources/cards/', _id, '.jpg'], _path),
			((get(@_reserveSlot, member, scaled_bitmap, _currentReserve), free(_currentReserve));true),
			new(_reserve, scaled_bitmap(image(_path))),
			send(_reserve, scale, size(_reserveEdge, _reserveEdge))
		),
		(
			send(@_reserveSlot, append, _reserve)
		)
	), !.

gui_update_scoreboard_table(_agentId, _tokens, _cards, _score) :-
	sum_list(_tokens, _tokensTotal),
	atom_concat(_tokensTotal, ' / 10', _tokensTotalLabel),
	atomic_list_concat(['agent', _agentId, 'scoreboard'], _reference),
	
	nth1(1, _tokens, _token1),
	nth1(2, _tokens, _token2),
	nth1(3, _tokens, _token3),
	nth1(4, _tokens, _token4),
	nth1(5, _tokens, _token5),
	nth1(6, _tokens, _token6),

	nth1(1, _cards, _card1),
	nth1(2, _cards, _card2),
	nth1(3, _cards, _card3),
	nth1(4, _cards, _card4),
	nth1(5, _cards, _card5),

	new(_table, tabular),
	send(_table, border, 0),
	send(_table, cell_spacing, 3),
	send(_table, rules, all),
	send_list(_table,
		[	append('Score', bold, center),
			append(_score, bold, center, colspan := 6),
			next_row,
			append('#Tokens', bold, center),
			append(_tokensTotalLabel, bold, center, colspan := 6),
			next_row,
			append('Tokens', bold, center),
			append(_token1, bold, center, background := white),
			append(_token2, bold, center, background := midnightblue, colour := white),
			append(_token3, bold, center, background := forestgreen, colour := white),
			append(_token4, bold, center, background := firebrick, colour := white),
			append(_token5, bold, center, background := sienna, colour := white),
			append(_token6, bold, center, background := gold),
			next_row,
			append('Cards', bold, center),
			append(_card1, bold, center, background := white),
			append(_card2, bold, center, background := midnightblue, colour := white),
			append(_card3, bold, center, background := forestgreen, colour := white),
			append(_card4, bold, center, background := firebrick, colour := white),
			append(_card5, bold, center, background := sienna, colour := white),
			append('0', bold, center, background := gold)
		]),
	((get(@_reference, member, tabular, _currentTable), free(_currentTable));true),
	send(@_reference, append, _table),
	!.

% USE pce_global
free_handles() :-
	(free(@agent1scoreboard);true),
	(free(@agent2scoreboard);true),
	(free(@agent3scoreboard);true),
	(free(@agent4scoreboard);true),

	(free(@agent1reserve1);true),
	(free(@agent1reserve2);true),
	(free(@agent1reserve3);true),
	(free(@agent2reserve1);true),
	(free(@agent2reserve2);true),
	(free(@agent2reserve3);true),
	(free(@agent3reserve1);true),
	(free(@agent3reserve2);true),
	(free(@agent3reserve3);true),
	(free(@agent4reserve1);true),
	(free(@agent4reserve2);true),
	(free(@agent4reserve3);true),
	
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
	(free(@token1count);true),
	(free(@token2count);true),
	(free(@token3count);true),
	(free(@token4count);true),
	(free(@token5count);true),
	(free(@token6count);true),

	(free(@scoreBoard);true),
	(free(@board);true),
	!.

create_board(_agents) :-
	free_handles(),
	new(@board, dialog('Splendor')),
	send(@board, icon, new(_, bitmap('./resources/splendor_icon.xpm'))),
	create_scoreboard(_agents),
	proper_length(_agents, _nAgent),
	create_reserve_area(_nAgent),
	create_card_area,
	create_token_area,
	create_noble_area(_agents),
	create_focus_area,
	send(@board, open),
	draw_tokens,
	create_tier_images,
	!.

create_scoreboard(_agents) :-
	area_size(_areaSize),% reserve_edge(_reserveEdge),
	length(_agents, _length),

	send(@board, append, new(@scoreBoard, dialog_group('Score Board', group)), next_row),
	% MARK PLAYER FRAMES WITH PEN
	foreach(
		(
			between(1, _length, _index), 
			atomic_list_concat(['agent', _index, 'scoreboard'], _agentArea),
			nth1(_index, _agents, _agentName),
			send(@scoreBoard, append, new(_area, dialog_group(_agentName, box))),
			send(_area, append, new(@_agentArea, dialog(size, _areaSize)), next_row)
			%send(_area, append, new(_agentReserves, dialog_group('Reserves', box)), next_row)
		), 
		(
			%% foreach(
			%% 	(
			%% 		between(1, 3, _indexReserve),
			%% 		atomic_list_concat(['agent', _index, 'reserve', _indexReserve], _reserveSlot)
			%% 	),
			%% 	(
			%% 		_indexReserve = 1 ->
			%% 		send(_area, append, 
			%% 			%new(@_reserveSlot, dialog(size, size(_reserveEdge, _reserveEdge))), 
			%% 			new(@_reserveSlot, dialog_group('', group)),
			%% 			next_row);
			%% 		send(_area, append, 
			%% 			%new(@_reserveSlot, dialog(size, size(_reserveEdge, _reserveEdge))), 
			%% 			new(@_reserveSlot, dialog_group('', group)),
			%% 			right)
			%% 	)
			%% )
		true
		)
	), !.

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

	
	!.

create_reserve_area(_agentCount) :-
	send(@scoreBoard, append, new(_reserves, dialog_group('reserves', group)), next_row),
	
	reserve_edge(_reserveEdge),
	_reserveSize = size(_reserveEdge, _reserveEdge),
	_gapSize = size(_reserveEdge / 10, _reserveEdge / 10),
	_boxSize = size(_reserveEdge * 34 / 10, _reserveEdge * 12 / 10),

	foreach(
		(
			between(1, _agentCount, _agentId),
			send(_reserves, append, new(_reserveBox, dialog_group('', box)), next_row)
		),
		(
			send(_reserveBox, gap, _gapSize),
			send(_reserveBox, size, _boxSize),
			foreach(
					(
						between(1, 3, _reserveId),
						atomic_list_concat(['agent', _agentId, 'reserve', _reserveId], _reference)
					),
					(
						send(_reserveBox, append, new(@_reference, dialog(size, _reserveSize)), right)
					)
			)			
		)
	),
!.

create_tier_images :-
	card_edge(_cardEdge),
	_cardSize = size(_cardEdge, _cardEdge),
	
	new(_tier3slot0, scaled_bitmap(image('./resources/tier3_2.jpg'))),
	new(_tier2slot0, scaled_bitmap(image('./resources/tier2_2.jpg'))),
	new(_tier1slot0, scaled_bitmap(image('./resources/tier1_2.jpg'))),

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
	send(_tokens, size, size(_tokenEdge * 23 / 10, _tokenEdge * 67 / 10)),
	
	foreach(
		(
			between(1, 6, _index),
			atomic_list_concat(['token', _index], _tokeni),
			atomic_list_concat(['token', _index, 'count'], _tokenicount),
			send(_tokens, append, new(@_tokeni, dialog(size, size(_tokenEdge * 13 / 10, _tokenEdge))), next_row),
			send(_tokens, append, new(@_tokenicount, dialog(size, size(_tokenEdge * 7 / 10, _tokenEdge))), right),
			true
			),
		(
			true
			)
		),
	!.

gui_update_tokens_board(_tokens) :-
	!,
	font_size(_fontSize),

	nth1(1, _tokens, _token1),
	((get(@token1count, member, text, _currentToken1), free(_currentToken1));true),
	new(_token1text, text(_token1)),
	send(_token1text, font, font(screen, bold, _fontSize)),
	send(_token1text, colour, white),
	send(@token1count, append, _token1text),

	nth1(2, _tokens, _token2),
	((get(@token2count, member, text, _currentToken2), free(_currentToken2));true),
	new(_token2text, text(_token2)),
	send(_token2text, font, font(screen, bold, _fontSize)),
	send(_token2text, colour, midnightblue),
	send(@token2count, append, _token2text),

	nth1(3, _tokens, _token3),
	((get(@token3count, member, text, _currentToken3), free(_currentToken3));true),
	new(_token3text, text(_token3)),
	send(_token3text, font, font(screen, bold, _fontSize)),
	send(_token3text, colour, forestgreen),
	send(@token3count, append, _token3text),

	nth1(4, _tokens, _token4),
	((get(@token4count, member, text, _currentToken4), free(_currentToken4));true),
	new(_token4text, text(_token4)),
	send(_token4text, font, font(screen, bold, _fontSize)),
	send(_token4text, colour, firebrick),
	send(@token4count, append, _token4text),

	nth1(5, _tokens, _token5),
	((get(@token5count, member, text, _currentToken5), free(_currentToken5));true),
	new(_token5text, text(_token5)),
	send(_token5text, font, font(screen, bold, _fontSize)),
	send(_token5text, colour, sienna),
	send(@token5count, append, _token5text),

	nth1(6, _tokens, _token6),
	((get(@token6count, member, text, _currentToken6), free(_currentToken6));true),
	new(_token6text, text(_token6)),
	send(_token6text, font, font(screen, bold, _fontSize)),
	send(_token6text, colour, gold),
	send(@token6count, append, _token6text),
	!.



gui_write_card_left(0, _).
gui_write_card_left(_tier, 0) :-
	font_size(_fontSize), card_edge(_cardEdge),

	atomic_list_concat(['tier', _tier, 'slot0'], _reference),

	((get(@_reference, member, scaled_bitmap, _currentCard), free(_currentCard));true),
	new(_card, scaled_bitmap(image('./resources/tier0_2.jpg'))),
	send(_card, scale, size(_cardEdge, _cardEdge)),
	
	send(@_reference, append, _card),

	((get(@_reference, member, text, _currentCount), free(_currentCount));true),
	new(_text, text('00')),
	send(_text, font, font(screen, bold, _fontSize)),
	send(_text, colour, firebrick),
	send(@_reference, display, _text, point(_cardEdge * 3 / 10, _cardEdge * 3 / 10)),
	!.
gui_write_card_left(_tier, _cardCount) :-
	font_size(_fontSize), card_edge(_cardEdge),
	atomic_list_concat(['tier', _tier, 'slot0'], _reference),
	((get(@_reference, member, text, _currentCount), free(_currentCount));true),
	(_cardCount < 10 -> atom_concat('0', _cardCount, _cardText);_cardText = _cardCount),
	new(_text, text(_cardText)),
	send(_text, font, font(screen, bold, _fontSize)),
	send(_text, colour, firebrick),
	send(@_reference, display, _text, point(_cardEdge * 3 / 10, _cardEdge * 3 / 10)),
	!.	

create_noble_area(_agents) :-
	proper_length(_agents, _agentCount),
	_nobleCount is _agentCount + 1,

	noble_edge(_nobleEdge),
	send(@board, append, new(_nobles, dialog_group('Nobles', box)), right),

	send(_nobles, gap, size(_nobleEdge / 10, _nobleEdge / 10)),
	send(_nobles, size, size(_nobleEdge * 12 / 10, _nobleEdge * (11 * _nobleCount + 1) / 10)),
	_nobleSize = size(_nobleEdge, _nobleEdge),

	foreach(
		(
			between(1, _nobleCount, _index),
			atomic_list_concat(['tier', 0, 'slot', _index], _reference),
			send(_nobles, append, new(@_reference, dialog(size, _nobleSize)), next_row),
			true
			),
		(
			true
			)
		),
	!.

mark_noble(_agentName, _position) :-
	get_color(_agentName, _color), !,
	atomic_list_concat(['tier', 0, 'slot', _position], _reference),
	new(_text, text(_agentName)),
	send(_text, font, font(screen, bold, 36)),
	send(_text, colour, _color),
	send(_text, pen, 3),
	send(@_reference, display, _text, point(0, 0)),
	!.

get_color(_agentName, _color) :-
	agents(_agents),
	nth1(_index, _agents, _agentName), !,
	(_index = 1 -> _color = 'cyan';
	_index = 2 -> _color = 'magenta';
	_index = 3 -> _color = 'yellow';
	_index = 4 -> _color = 'white';
	_color = 'green'), !.
get_color(_, 'green').

create_focus_area :-
	%send(@board, append, new(_currentAgent, dialog_group('Current Agent', box)), next_row),
	
	%_nullTokens = '0 : 0 / 0 : 0 / 0 : 0 / 0 : 0 / 0 : 0 / 0', % atomic_list_concat([0,0,0,0,0,0], ' / ', X)
	%send(_currentAgent, append, new(_currentAgentScore, label(text, 'Score: 0'))),
	%send(_currentAgent, append, new(_currentAgentChips, label(text, 'Chips: 0/10')), right),
	%send(_currentAgent, append, new(_currentAgentTokens, label(text, _nullTokens)), right),
	%send(_currentAgent, append, new(_currentAgentReserves, label(text, 'Reserves')), right),
	true.