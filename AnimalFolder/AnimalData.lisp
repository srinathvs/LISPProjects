(setq *nodes*
'
((14 "Does it have a vile spit" LAMA CAMEL)
 (13 "does it resemble a toad but have slimy moist skin" FROG TOAD)
 (12 "Is it used to make pork" PIG COW)
 (11 "It has tusks, but resembles a pig" BOAR (12))
 (10 "Does it have a trunk and tusks" ELEPHANT (11))
 (9 "It can change its color to adapt to its surroundings" CHAMELEON LIZARD)
 (8 "Does it lie in the water like an unassuming log" CROCODILE (9))
 (7 "Does it hiss" SNAKE (8)) (6 "does it stay in the desert" (14) (10))
 (5 "Is a beast of burden" DONKEY (6))
 (4 "Does it have warts over its skin" (13) (7)) (3 "does it meow" CAT (5))
 (2 "it barks" DOG (3)) (THING "Is this thing a mammal" (2) (4))))
(setq *node-count* 14)
