:- use_module(library(pce)).
:- use_module(library(scaledbitmap)).

%:- initialization(draw_empty_board()).

%(send(+Receiver,+Selector(...Args...)))

% _w1hiteLeft, _blueLeft, _greenLeft, _redLeft, _blackLeft

	%(free(_agent1Area);true),
	%(free(_agent2Area);true),
	%(free(_agent3Area);true),
	%(free(_agent4Area);true).

test :-
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

update_card(_id, _tier, _position) :-
	atomic_list_concat(['./resources/', _id, '.jpg'], _path),
	atomic_list_concat(['tier', _tier, 'slot', _position], _reference),
	
	((get(@_reference, member(bitmap), _currentCard), free(_currentCard));true),
	new(_card, scaled_bitmap(image(_path))),
	send(_card, scale, size(75, 100)),
	send(@_reference, append, _card).


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
	(free(@board);true).

draw :-
	free_handles(),
	_nullTokens = '0 : 0 / 0 : 0 / 0 : 0 / 0 : 0 / 0 : 0 / 0', % atomic_list_concat([0,0,0,0,0,0], ' / ', X)
	
	new(@board, dialog('Splendor')),

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
	send(_agent4Area, append, new(_agent4Reserves, label(text, 'Reserves')), next_row),
	
	send(@board, append, new(_decks, dialog_group('cards', group)), right),

	send(_decks, append, new(_tier3, dialog_group('Tier 3', box))),
	send(_decks, append, new(_tier2, dialog_group('Tier 2', box)), next_row),
	send(_decks, append, new(_tier1, dialog_group('Tier 1', box)), next_row),

	
	
	send(_tier3, append, new(@tier3slot0, dialog)),
	send(_tier3, append, new(@tier3slot1, dialog), right),
	send(_tier3, append, new(@tier3slot2, dialog), right),
	send(_tier3, append, new(@tier3slot3, dialog), right),
	send(_tier3, append, new(@tier3slot4, dialog), right),
	

	send(_tier2, append, new(@tier2slot0, dialog)),
	send(_tier2, append, new(@tier2slot1, dialog), right),
	send(_tier2, append, new(@tier2slot2, dialog), right),
	send(_tier2, append, new(@tier2slot3, dialog), right),
	send(_tier2, append, new(@tier2slot4, dialog), right),

	send(_tier1, append, new(@tier1slot0, dialog)),
	send(_tier1, append, new(@tier1slot1, dialog), right),
	send(_tier1, append, new(@tier1slot2, dialog), right),
	send(_tier1, append, new(@tier1slot3, dialog), right),
	send(_tier1, append, new(@tier1slot4, dialog), right),

	send(@board, append, new(_tokens, dialog_group('Tokens', box)), right),

	send(_tokens, append, new(_tokenWhite, label(text, '7')), next_row),
	send(_tokens, append, new(_tokenBlue, label(text, '7')), next_row),
	send(_tokens, append, new(_tokenGreen, label(text, '7')), next_row),
	send(_tokens, append, new(_tokenRed, label(text, '7')), next_row),
	send(_tokens, append, new(_tokenBlack, label(text, '7')), next_row),
	send(_tokens, append, new(_tokenYellow, label(text, '5')), next_row),	

	send(@board, append, new(_nobles, dialog_group('Nobles', box)), right),
	
	send(_nobles, append, new(_noble1, label(text, 'Noble 1')), next_row),
	send(_nobles, append, new(_noble2, label(text, 'Noble 2')), next_row),
	send(_nobles, append, new(_noble3, label(text, 'Noble 3')), next_row),
	send(_nobles, append, new(_noble4, label(text, 'Noble 4')), next_row),
	send(_nobles, append, new(_noble5, label(text, 'Noble 5')), next_row),

	send(@board, append, new(_currentAgent, dialog_group('Current Agent', box)), next_row),
	
	send(_currentAgent, append, new(_currentAgentScore, label(text, 'Score'))),
	send(_currentAgent, append, new(_currentAgentChips, label(text, 'Chips: 0/10')), right),
	send(_currentAgent, append, new(_currentAgentTokens, label(text, _nullTokens)), right),
	send(_currentAgent, append, new(_currentAgentReserves, label(text, 'Reserves')), right),

	send(@board, open),
	!.