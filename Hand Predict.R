# ===========================
#   
#   Author: Steven Cheng
#   Date: 10/24/2022
#   
#   Hand Predict
#   Work backwards and try to figure out what's left of "Deck" and try to pick
#   out best hand. Want to get out Chance of getting FH or Straight | what information we know.
#
# ===========================

source("Libraries.R")
source("Hand Select.R")
source("Total Hand Value.R")

Hand <- DealFromHoldem()[[1]]

plan(multisession, workers = length(availableWorkers()) -2)

# Maximal Hand Value ------------------------------------------------------

# We will need to determine whether we are on the "pre-flop, flop, or turn." The
# In the case of only 2 players, the number of possible hands will be
# choose(Deck - 2x Play


# Post flop, we have choose(Deck - 2x Players - 3, 2) = 990
# Post turn, we have choose(Deck - 2x Players - 4, 1) = 44
# Pre Flop

Hand[1:2,]

# Map every possible pair and determine W/L using simulation method?


# Flop
Hand[1:5,]



# Turn
Hand[1:6,]

# Expected Hand value -----------------------------------------------------

# We will want to calculate the expected hand strength given "flop, river, or turn"


# We will need to determine whether we are on the "pre-flop, flop, or turn." The
# In the case of only 2 players, the number of possible hands will be
# choose(Deck - 2x Players, 5) = 1,712,304. Possible number of starting pairs is
# choose(Deck,2) = 1326

# Post flop, we have choose(Deck - 2x Players - 3, 2) = 990
# Post turn, we have choose(Deck - 2x Players - 4, 1) = 44

# Pre Flop
Hand[1:2,]

# Map every possible pair and determine W/L using simulation method?

# Not sure how to get rid of the different combinations. Right now it is
# computing permutations

# AllHands <-
#   expand_grid(Deck$Card, Deck$Card) %>% 
#   transmute(., rename(., c("H1" = names(.)[1], "H2" = names(.)[2]))) %>% 
#   filter(!(H1 == H2)) %>% 
#   mutate(., S1 = str_c(H1, H2), S2 = str_c(H2,H1))


# The below code iterates through the entire Deck and generates all possible combo's
x <- Deck$Card
PreFlopHands <- tibble(i = character(), x = character())

for( i in x){
  x <- x[-match(i, x)]
  y <- expand_grid(i, x)
  PreFlopHands <- add_row(PreFlopHands, y)
  
}


# Simulate 1000 Hands
PreFlopSim <- tibble(y = numeric(), hand = character(), x = list())

system.time(
for( i in 1:500){
 x<-DealFromHoldem(1)
 y<-TotalValue(x[[1]])[[1]]
 hand <- FindHand(x[[1]])
 PreFlopSim <- add_row(PreFlopSim, y = y,hand = hand[[1]] ,x = list(x))
 print(i)
}
)

# These times seem independent of the function I use and more about how fast my
# computer runs. I think the next option is to use map or apply function and not for loop.
# 8.22, 8.46, 8.03, 7.97, 8.21 
# 7.97, 8.28, 8.31. 8.69, 8.46

# Saving down the first iteration.

saveRDS(PreFlopSim, file = "./Backup/PreFlopSim1") 

# Currently takes ~200 seconds to simulate 1000 hands.
# Assuming we want each hand simulated x100, we will need 1326*30s = 39780s, or
# roughly 7-11 hours. Computationally unreasonable to do on the fly.

# Need to look into whether vectorizing the for loop would make it faster?

# Solution: 
# 1) run it anyways
# 2) Optimize functions and rerun: Potentially use the sample function and then re-map to hands. 
# 3) Parallel compute
# 4) Leverage Holdem package somehow?


m <- PreFlopSim %>% 
  group_by(., hand) %>% 
  summarise(.,min = min(y), max = max(y)) %>% 
  arrange(., min)
  # Quick check on the bounds of our data.


# two hand extract PR, need to map the first two values of PreFlopSim to
# PreFlopHands.


# PreFlopSim[[3]][[n]][c(1,2),]
x <- PreFlopSim[[3]]
x <-lapply(x, slice, c(1,2))



# Flop --------------------------------------------------------------------

Hand[1:5,]




# Turn --------------------------------------------------------------------

Hand[1:6,]

