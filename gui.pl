

card_edge(200).
noble_edge(120).
token_edge(80).
area_size(size(160, 100)).


update_tokens(_tokens) :-
	foreach((between(1, 6, _index), 
		atomic_list_concat(['token', _index, 'count'], _reference),
		nth1(_index, _tokens, _newToken),
		atom_concat('\n', _newToken, _newTokenLabel)
		), 
		(	
			send(@_reference, selection, _newTokenLabel)
		)),
	!.


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

% cyan, magenta, yellow, white, purple, brown

mark_card(_tier, _position) :-
	atomic_list_concat(['tier', _tier, 'slot', _position], _reference),
	get(@_reference, member, scaled_bitmap, _card),
	send_list(_card, [pen(5), colour(green)]),
	!.

unmark_card(_tier, _position) :-
	atomic_list_concat(['tier', _tier, 'slot', _position], _reference),
	get(@_reference, member, scaled_bitmap, _card),
	send_list(_card, [pen(0)]),
	!.

update_card(_id, _tier, _position) :-
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

update_scoreboard_table(_agentId, _tokens, _cards, _score) :-
	sum_list(_tokens, _tokensTotal),
	atom_concat(_tokensTotal, ' / 10', _tokensTotalLabel),
	atomic_list_concat(['agent', _agentId, 'area'], _reference),
	
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
		[ append('Score', bold, center),
			append(_score, bold, center, colspan := 6),
			next_row,
			append('#Tokens', bold, center),
			append(_tokensTotalLabel, bold, center, colspan := 6),
			next_row,
			append('Tokens', bold, center),
			append(_token1, bold, center, background := wheat),
			append(_token2, bold, center, background := midnightblue, colour := white),
			append(_token3, bold, center, background := seagreen),
			append(_token4, bold, center, background := firebrick),
			append(_token5, bold, center, background := black, colour := white),
			append(_token6, bold, center, background := gold),
			next_row,
			append('Cards', bold, center),
			append(_card1, bold, center, background := wheat),
			append(_card2, bold, center, background := midnightblue, colour := white),
			append(_card3, bold, center, background := seagreen),
			append(_card4, bold, center, background := firebrick),
			append(_card5, bold, center, background := black, colour := white),
			append('0', bold, center, background := gold)
		]),
	((get(@_reference, member, tabular, _currentTable), free(_currentTable));true),
	send(@_reference, append, _table),
	!.

% USE pce_global
free_handles() :-
	(free(@agent1area);true),
	(free(@agent1reserves);true),
	(free(@agent2area);true),
	(free(@agent2reserves);true),
	(free(@agent3area);true),
	(free(@agent3reserves);true),
	(free(@agent4area);true),
	(free(@agent4reserves);true),
	
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
	(free(@board);true),
	!.

create_board(_agents) :-
	free_handles(),
	new(@board, dialog('Splendor')),
	create_scoreboard(_agents),
	create_card_area,
	create_token_area,
	create_noble_area,
	create_focus_area,
	send(@board, open),
	!.

test :-
	
	update_scoreboard_table(1, [0,0,0,0,0,0], [0,0,0,0,0], 0),
	update_scoreboard_table(2, [0,0,0,0,0,0], [0,0,0,0,0], 0),
	update_scoreboard_table(3, [0,0,0,0,0,0], [0,0,0,0,0], 0),
	update_scoreboard_table(4, [0,0,0,0,0,0], [0,0,0,0,0], 0),

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

	mark_card(3,0),

	!.

create_scoreboard(_agents) :-
	area_size(_areaSize),
	length(_agents, _length),

	send(@board, append, new(_scoreBoard, dialog_group('Score Board', group)), next_row),
	% MARK PLAYER FRAMES WITH PEN
	foreach((between(1, _length, _index), 
		atomic_list_concat(['agent', _index, 'area'], _agentArea),
		nth1(_index, _agents, _agentName),
		send(_scoreBoard, append, new(_area, dialog_group(_agentName, box)))
		), 
		(
			send(_area, append, new(@_agentArea, dialog(size, _areaSize)), next_row)
		)),
	%new(_logo, scaled_bitmap(image('./resources/lgo_Splendor.xpm'))),
	%send(_logo, scale, size(180, 72)),
	%send(_scoreBoard, append, _logo, next_row),
	!.

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
	
	send(@tier3slot0, append, _tier3slot0),
	send(@tier2slot0, append, _tier2slot0),
	send(@tier1slot0, append, _tier1slot0),

	!.

create_token_area :-
	token_edge(_tokenEdge),

	send(@board, append, new(_tokens, dialog_group('Tokens', box)), right),
	send(_tokens, gap, size(_tokenEdge / 10, _tokenEdge / 10)),
	send(_tokens, size, size(_tokenEdge * 23 / 10, _tokenEdge * 67 / 10)),
	
	_tokenSize = size(_tokenEdge, _tokenEdge),

	foreach(
		(
			between(1, 6, _index),
			atomic_list_concat(['./resources/tokens/token', _index, '.jpg'], _path),
			atomic_list_concat(['token', _index], _tokeni),
			atomic_list_concat(['token', _index, 'count'], _tokenicount),
			new(_tokenBitmap, scaled_bitmap(image(_path))),
			send(_tokens, append, new(@_tokeni, dialog(size, _tokenSize)), next_row),			
			send(_tokens, append, new(_tokencount, dialog(size, _tokenSize)), right),
			send(_tokencount, append, new(@_tokenicount, label(text, '\n 0')))
			),
		(
			true
			)
		),
	!.

	%draw_tokens.
	

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
	send(_currentAgent, append, new(_currentAgentScore, label(text, 'asd \u2460 asd'))),
	send(_currentAgent, append, new(_currentAgentChips, label(text, 'Chips: 0/10')), right),
	send(_currentAgent, append, new(_currentAgentTokens, label(text, _nullTokens)), right),
	send(_currentAgent, append, new(_currentAgentReserves, label(text, 'Reserves')), right).