:- dynamic tokens/2.
:- dynamic cards/2.
:- dynamic nobles/2.
:- dynamic reserves/2.
:- dynamic prestige/2.
:- dynamic agent/1.

delete_db :-
	retractall(tokens(_, _)),
	retractall(cards(_, _)),
	retractall(nobles(_, _)),
	retractall(reserves(_, _)),
	retractall(prestige(_, _)),
	retractall(agent(_)),
	retractall(agents(_)),
	!.

init_db :-
	make,
	assert(tokens(board, [0,0,0,0,0,0])),
	consult(deck),
	
	load_modules,
	!.

load_modules :-
	consult(agents),
	agents(_agents),
	proper_length(_agents, _nAgent),
	forall(
		(
			between(1, _nAgent, _i),
			nth1(_i, _agents, _agent)
		),
		(
			atomic_list_concat(['./agents/', _agent], _modulePath),
			use_module(_modulePath),
			assert(tokens(_agent, [0, 0, 0, 0, 0, 0])),
			assert(cards(_agent, [])),
			assert(nobles(_agent, [])),
			assert(reserves(_agent, [])),
			assert(prestige(_agent, 0))
		)
	),
	!.

update_tokens(_owner, _tokens) :-
	(retract(tokens(_owner, _));true),
	assert(tokens(_owner, _tokens)),
	!.

update_cards(_owner, _cards) :-
	(retract(cards(_owner, _));true),
	assert(cards(_owner, _cards)),
	!.

update_nobles(_owner, _nobles) :-
	(retract(nobles(_owner, _));true),
	assert(nobles(_owner, _nobles)),
	!.

update_reserves(_owner, _reserves) :-
	(retract(reserves(_owner, _));true),
	assert(reserves(_owner, _reserves)),
	!.

update_prestige(_owner, _prestige) :-
	(retract(prestige(_owner, _));true),
	assert(prestige(_owner, _prestige)),
	!.

reset_db :-
	delete_db,
	init_db.

print_tokens :-
	listing(tokens).
print_tokens(_owner) :-
	listing(tokens(_owner, _)).

print_cards :-
	listing(cards).
print_cards(_owner) :-
	listing(cards(_owner, _)).

print_nobles :-
	listing(nobles).
print_nobles(_owner) :-
	listing(nobles(_owner, _)).

print_reserves :-
	listing(reserves).
print_reserves(_owner) :-
	listing(reserves(_owner, _)).

print_prestige :-
	listing(prestige).
print_prestige(_owner) :-
	listing(prestige(_owner, _)).

print_agents :-
	listing(agents).

print_agent :-
	listing(agent).

print_db :-
	print_tokens,
	print_cards,
	print_nobles,
	print_reserves,
	print_prestige.
print_db(_owner) :-
	print_tokens(_owner),
	print_cards(_owner),
	print_nobles(_owner),
	print_reserves(_owner),
	print_prestige(_owner).