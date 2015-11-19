:- dynamic deck1/1.
:- dynamic deck2/1.
:- dynamic deck3/1.
:- dynamic nobles/1.
:- dynamic area1/1.
:- dynamic area2/1.
:- dynamic area3/1.
:- dynamic coins/1.
:- dynamic playerCoins/1.
:- dynamic playerCards/1.

:- retractall(deck1(_)).
:- retractall(deck2(_)).
:- retractall(deck3(_)).
:- retractall(area1(_)).
:- retractall(area2(_)).
:- retractall(area3(_)).
:- retractall(nobles(_)).
:- retractall(coins(_)).
:- retractall(playerCoins(_)).
:- retractall(playerCards(_)).

:- include(deck).

agents([bot1, bot2, bot3, bot4]).

resetDB() :-
	retractall(deck1(_)),
	retractall(deck2(_)),
	retractall(deck3(_)),
	retractall(area1(_)),
	retractall(area2(_)),
	retractall(area3(_)),
	retractall(nobles(_)),
	retractall(coins(_)),
	retractall(playerCoins(_)),
	retractall(playerCards(_)).


start() :-
	initialization().

initialization() :-
	agents(Agents),
	proper_length(Agents, N_Player), !,
	(N_Player == 2 -> (N_Coin is 4, !);
	N_Player == 3 -> (N_Coin is 5, !);
	N_Player == 4 -> (N_Coin is 7, !);
	false),

	N_Noble is N_Player + 1,
	
	deck1(Tier1),
	deck2(Tier2),
	deck3(Tier3),
	nobles(AllNobles),

	empty_list_of_length_N(4, Null1),
	empty_list_of_length_N(4, Null2),
	empty_list_of_length_N(4, Null3),
	empty_list_of_length_N(N_Noble, Null4),
	
	draw_N_cards(4, Tier1, Null1, Deck1, Area1), !,
	draw_N_cards(4, Tier2, Null2, Deck2, Area2), !,
	draw_N_cards(4, Tier3, Null3, Deck3, Area3), !,
	draw_N_cards(N_Noble, AllNobles, Null4, _, Nobles), !,

	resetDB(),

	assert(deck1(Deck1)), !,
	assert(deck2(Deck2)), !,
	assert(deck3(Deck3)), !,
	assert(area1(Area1)), !,
	assert(area2(Area2)), !,
	assert(area3(Area3)), !,
	assert(nobles(Nobles)), !,
	assert(coins([N_Coin, N_Coin, N_Coin, N_Coin, N_Coin, 3])), !,

	initializePlayers(N_Player).


run(Deck1, Deck2, Deck3, Area1, Area2, Area3, Nobles) :- 
	display(Deck1),
	display(Deck2),
	display(Deck3),
	display(Area1),
	display(Area2),
	display(Area3),
	display(Nobles).

%gameDidNotEnd(state),
%		whoseTurnIsIt(index, newIndex),
%		whatIsYourAction(state, newIndex, action),
%		applyAction(state, action),
%		run().


draw_N_cards(_, [], To, [], To).
draw_N_cards(0, From, To, From, To).
draw_N_cards(N, From, To, Rest, Result) :-
	N > 0,
	M is N-1,
	random_select(X, From, Remainder),
	select(null, To, X, Acc),
	draw_N_cards(M, Remainder, Acc, Rest, Result).

empty_list_of_length_N(0, []).
empty_list_of_length_N(1, [null]).
empty_list_of_length_N(N, [null|List]) :-
	N > 0,
	M is N-1,
	empty_list_of_length_N(M, List).

list_of_length_N(0, _, []).
list_of_length_N(1, X, [X]) :- !.
list_of_length_N(N, X, [X|List]) :-
	N > 0,
	M is N-1,
	list_of_length_N(M, X, List).

initializePlayers(N) :-
	initializePlayerCoins(N),
	initializePlayerCards(N).

initializePlayerCoins(N) :-
	list_of_length_N(6, 0, L),
	populateList([], L, N, PlayerCoins), !,
	assert(playerCoins(PlayerCoins)), !.

initializePlayerCards(N) :-
	populateList([], [], N, PlayerCards), !,
	assert(playerCards(PlayerCards)), !.

populateList(Input, _, 0, Input).
populateList(Input, Parameter, 1, Result) :-
	append(Input, [Parameter], Result), !.
populateList(Input, Parameter, Count, Result) :-
	NewCount is Count-1,
	append(Input, [Parameter], NewInput),
	populateList(NewInput, Parameter, NewCount, Result).



take_coins(CoinList) :- 
	proper_length(CoinList, 5),
	coins(BoardCoins),
	nth0(0, BoardCoins, BoardWhite),
	nth0(1, BoardCoins, BoardBlue),
	nth0(2, BoardCoins, BoardGreen),
	nth0(3, BoardCoins, BoardRed),
	nth0(4, BoardCoins, BoardBlack),
	nth0(0, CoinList, White),
	nth0(1, CoinList, Blue),
	nth0(2, CoinList, Green),
	nth0(3, CoinList, Red),
	nth0(4, CoinList, Black),

	WhiteLeft is BoardWhite - White,
	BlueLeft is BoardBlue - Blue,
	GreenLeft is BoardGreen - Green,
	RedLeft is BoardRed - Red,
	BlackLeft is BoardBlack - Black,

	WhiteLeft >= 0,
	BlueLeft >= 0,
	GreenLeft >= 0,
	RedLeft >= 0,
	BlackLeft >= 0,

	sum_list(CoinList, SumCoinList),
	(SumCoinList == 2 -> Alfa is 0;
	SumCoinList == 3 -> Alfa is 1; false),

	(White > 0 -> BetaWhite is 1; BetaWhite is 0),
	(Blue > 0 -> BetaBlue is 1; BetaBlue is 0),
	(Green > 0 -> BetaGreen is 1; BetaGreen is 0),
	(Red > 0 -> BetaRed is 1; BetaRed is 0),
	(Black > 0 -> BetaBlack is 1; BetaBlack is 0),
	!,

	SumBeta is (BetaWhite + BetaBlue + BetaGreen + BetaRed + BetaBlack),
	(0 is (Alfa * (SumBeta-3)) + ((1-Alfa) * (SumBeta-1))),
	(0 is ((Alfa * BetaWhite * (White-1)) + ((1-Alfa) * BetaWhite * (White-2)) + ((1-BetaWhite) * White))),
	(0 is ((Alfa * BetaBlue * (Blue-1)) + ((1-Alfa) * BetaBlue * (Blue-2)) + ((1-BetaBlue) * Blue))),
	(0 is ((Alfa * BetaGreen * (Green-1)) + ((1-Alfa) * BetaGreen * (Green-2)) + ((1-BetaGreen) * Green))),
	(0 is ((Alfa * BetaRed * (Red-1)) + ((1-Alfa) * BetaRed * (Red-2)) + ((1-BetaRed) * Red))),
	(0 is ((Alfa * BetaBlack * (Black-1)) + ((1-Alfa) * BetaBlack * (Black-2)) + ((1-BetaBlack) * Black))),
	!,
	((0 is (1-Alfa));(0 is BetaWhite);(WhiteLeft >= 2)), !,
	((0 is (1-Alfa));(0 is BetaBlue);(BlueLeft >= 2)), !,
	((0 is (1-Alfa));(0 is BetaGreen);(GreenLeft >= 2)), !,
	((0 is (1-Alfa));(0 is BetaRed);(RedLeft >= 2)), !,
	((0 is (1-Alfa));(0 is BetaBlack);(BlackLeft >= 2)), !.


%white#,blue#,green#,red#,black#










