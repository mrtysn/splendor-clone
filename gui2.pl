:- use_module(library(pce)).
:- use_module(library(tabular)).
:- use_module(library(autowin)).

colour(white).
colour(red).
colour(green).
colour(blue).
colour(black).

go(_tokens, _cards, _score) :-
	sum_list(_tokens, _tokensTotal),
	atom_concat(_tokensTotal, ' / 10', _tokensTotalLabel),
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

	new(P, auto_sized_picture('Table with merged cells')),
	%new(P, dialog),
	send(P, display, new(T, tabular)),
	send(T, border, 0),
	send(T, cell_spacing, 3),
	send(T, rules, all),
	send_list(T,
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
	send(P, open).

		