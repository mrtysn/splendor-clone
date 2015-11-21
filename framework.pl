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
:- dynamic playerReserves/1.
:- dynamic currentPlayer/1.

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
:- retractall(currentPlayer(_)).

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
	retractall(playerCards(_)),
	retractall(currentPlayer(_)).

start() :-
	initialization(),
	run().

initialization() :-
	agents(Agents),
	proper_length(Agents, N_Player), !,
	(	
		N_Player == 2 -> N_Coin is 4;
		N_Player == 3 -> N_Coin is 5; 
		N_Player == 4 -> N_Coin is 7;
		false
	),

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


run() :- 
	currentPlayer(CurrentPlayer),
	agents(Agents),
	nth(CurrentPlayer, Agents, Agent),
	
	
	next_player().
	%run().

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
	(random_select(X, From, Remainder) -> 
		(select(null, To, X, Acc), draw_N_cards(M, Remainder, Acc, Rest, Result));
		true).

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
	assert(currentPlayer(1)),
	initializePlayerCoins(N),
	initializePlayerCards(N).

initializePlayerCoins(N) :-
	%list_of_length_N(6, 0, L),
	list_of_length_N(6, 7, L),
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
	min_member(MinCoin, CoinList),
	MinCoin is 0,
	nth0(0, CoinList, White),
	nth0(1, CoinList, Blue),
	nth0(2, CoinList, Green),
	nth0(3, CoinList, Red),
	nth0(4, CoinList, Black),

	coins(BoardCoins),
	nth0(0, BoardCoins, BoardWhite),
	nth0(1, BoardCoins, BoardBlue),
	nth0(2, BoardCoins, BoardGreen),
	nth0(3, BoardCoins, BoardRed),
	nth0(4, BoardCoins, BoardBlack),
	nth0(5, BoardCoins, BoardGold),
	
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

	(White > 0 -> BetaWhite is 1; BetaWhite is 0),
	(Blue > 0 -> BetaBlue is 1; BetaBlue is 0),
	(Green > 0 -> BetaGreen is 1; BetaGreen is 0),
	(Red > 0 -> BetaRed is 1; BetaRed is 0),
	(Black > 0 -> BetaBlack is 1; BetaBlack is 0),

	sum_list(CoinList, SumCoinList),
	SumBeta is (BetaWhite + BetaBlue + BetaGreen + BetaRed + BetaBlack),
	
	(
		((SumCoinList == 2), (SumBeta == 1)) -> (!, two_of_a_kind(BetaWhite, WhiteLeft, BetaBlue, BlueLeft, BetaGreen, GreenLeft, BetaRed, RedLeft, BetaBlack, BlackLeft));
		((SumCoinList == 3), (SumBeta == 3)) -> (!);
		(SumCoinList == 1) -> (!, no_coin_left(White, WhiteLeft, Blue, BlueLeft, Green, GreenLeft, Red, RedLeft, Black, BlackLeft));
		false
	), !,

	retract(coins(_)), !,
	assert(coins([WhiteLeft, BlueLeft, GreenLeft, RedLeft, BlackLeft, BoardGold])), !.


two_of_a_kind(BetaWhite, WhiteLeft, BetaBlue, BlueLeft, BetaGreen, GreenLeft, BetaRed, RedLeft, BetaBlack, BlackLeft):-
	((0 is BetaWhite);(WhiteLeft >= 2)), !,
	((0 is BetaBlue);(BlueLeft >= 2)), !,
	((0 is BetaGreen);(GreenLeft >= 2)), !,
	((0 is BetaRed);(RedLeft >= 2)), !,
	((0 is BetaBlack);(BlackLeft >= 2)), !.

no_coin_left(White, WhiteLeft, Blue, BlueLeft, Green, GreenLeft, Red, RedLeft, Black, BlackLeft):-
	(White == 0 -> WhiteLeft == 0; WhiteLeft < 3),
	(Blue == 0 -> BlueLeft == 0; BlueLeft < 3),
	(Green == 0 -> GreenLeft == 0; GreenLeft < 3),
	(Red == 0 -> RedLeft == 0; RedLeft < 3),
	(Black == 0 -> BlackLeft == 0; BlackLeft < 3).

next_player() :-
	agents(Agents),
	proper_length(Agents, N_Player),
	currentPlayer(CurrentPlayer),
	NextPlayer is ((CurrentPlayer+1) mod N_Player),
	retract(currentPlayer(_)),
	assert(currentPlayer(NextPlayer)).

purchase_card(Player, Tier, Position) :-
	playerCoins(PlayerCoins),
	nth1(Player, PlayerCoins, Coins),

	playerCards(PlayerCards),
	nth1(Player, PlayerCards, Cards),

	findall(Color, (member(PlayerCard, PlayerCards), nth1(3, PlayerCard, Color)), Colors),
	findall(true, member('white', Colors), Whites),
	length(Whites, WhiteCards),
	findall(true, member('blue', Colors), Blues),
	length(Blues, BlueCards),
	findall(true, member('green', Colors), Greens),
	length(Greens, GreenCards),
	findall(true, member('red', Colors), Reds),
	length(Reds, RedCards),
	findall(true, member('black', Colors), Blacks),
	length(Blacks, BlackCards),	
	
	nth1(1, Coins, WhiteCoins),
	nth1(2, Coins, BlueCoins),
	nth1(3, Coins, GreenCoins),
	nth1(4, Coins, RedCoins),
	nth1(5, Coins, BlackCoins),
	nth1(6, Coins, GoldCoins),

	between(1, 4, Position),
	(
		(Tier == 1) -> (area1(Area), deck1(Deck));
		(Tier == 2) -> (area2(Area), deck2(Deck));
		(Tier == 3) -> (area3(Area), deck3(Deck));
		false
	),
	nth1(Position, Area, Card),

	nth1(4, Card, WhiteCost),
	nth1(5, Card, BlueCost),
	nth1(6, Card, GreenCost),
	nth1(7, Card, RedCost),
	nth1(8, Card, BlackCost),

	WhiteLeft is (WhiteCoins + WhiteCards - WhiteCost),
	BlueLeft is (BlueCoins + BlueCards - BlueCost),
	GreenLeft is (GreenCoins + GreenCards - GreenCost),
	RedLeft is (RedCoins + RedCards - RedCost),
	BlackLeft is (BlackCoins + BlackCards - BlackCost),

	WhiteLeft >= 0,
	BlueLeft >= 0,
	GreenLeft >= 0,
	RedLeft >= 0,
	BlackLeft >= 0,

	% this is wrong
	WhiteCoinLeft is (WhiteCoins - WhiteCost + WhiteCards),
	BlueCoinLeft is (BlueCoins - BlueCost + BlueCards),
	GreenCoinLeft is (GreenCoins - GreenCost + GreenCards),
	RedCoinLeft is (RedCoins - RedCost + RedCards),
	BlackCoinLeft is (BlackCoins - BlackCost + BlackCards),

	CoinsNew = [WhiteCoinLeft, BlueCoinLeft, GreenCoinLeft, RedCoinLeft, BlackCoinLeft, GoldCoins],

	append(Cards, [Card], CardsNew),
	select(Cards, PlayerCards, CardsNew, PlayerCardsNew),
	select(Coins, PlayerCoins, CoinsNew, PlayerCoinsNew),
	select(Card, Area, null, AreaTemp),
	draw_N_cards(1, Deck, AreaTemp, DeckNew, AreaNew), !,

	retract(playerCards(_)),
	assert(playerCards(PlayerCardsNew)),
	retract(playerCoins(_)),
	assert(playerCoins(PlayerCoinsNew)),

	(
		(Tier == 1) -> (
			retract(deck1(_)),
			assert(deck1(DeckNew)),
			retract(area1(_)),
			assert(area1(AreaNew))
		);
		(Tier == 2) -> (
			retract(deck2(_)),
			assert(deck2(DeckNew)),
			retract(area2(_)),
			assert(area2(AreaNew))
		);
		(Tier == 3) -> (
			retract(deck3(_)),
			assert(deck3(DeckNew)),
			retract(area3(_)),
			assert(area3(AreaNew))
		);
		false
	).

	
%To pay the cost, first the player has to 
%look for cost reductions provided by 
%his built developments, and only pay the 
%surplus with gem chips