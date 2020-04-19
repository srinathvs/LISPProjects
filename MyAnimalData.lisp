(setq *nodes*
'
((12 "Is it used to make pork" PIG COW)
 (11 "It has tusks, but resembles a pig" BOAR (12))
 (10 "Does it have a trunk and tusks" ELEPHANT (11))
 (9 "It can change its color to adapt to its surroundings" CHAMELEON LIZARD)
 (8 "Does it lie in the water like an unassuming log" CROCODILE (9))
 (7 "Does it hiss" SNAKE (8)) (6 "does it stay in the desert" CAMEL (10))
 (5 "Is a beast of burden" DONKEY (6))
 (4 "Does it have warts over its skin" TOAD (7)) (3 "does it meow" cat (5))
 (2 "it barks" DOG (3)) (THING "Is this thing a mammal" (2) (4))))
(setq *node-count* 12)
