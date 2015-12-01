
This is a prolog implementation of the card game Splendor.

In order to be able to play this game, you need to implement your own agents or you can use the human interface. There is also a random bot included.

You can modify agents.pl in order to specify which agents are going to play the game.

You can use the human interface via the command line. At every turn, the framework will ask you your action.

Actions consist of action types and action parameters.
Action types are 1, 2 and 3 for taking tokens, purchasing development cards and, reserving cards respectively.

Action parameter for taking tokens is [c{1}, c{2}, c{3}, c{4}, c{5}] where c{i} are the number of tokens you are willing to take.

Action parameter for purchasing cards is [Tier, Position, [c{1}, c{2}, c{3}, c{4}, c{5}, c{6}]] where Tier, Position denote the card and Tokens are the tokens you are offering in order to purchase the card.

Action parameter for reserving a card is [Tier, Position] which specifies the card you are willing to reserve.

You can launch the game by starting the prolog session, consulting(framework), typing "init." and "loop.".

Humans and agents use the same method for giving commands to the frame.