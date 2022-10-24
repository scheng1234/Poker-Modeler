# ===========================
#   
#   Author: Steven Cheng
#   Date: 10/14/2022
#   
#   Hand Calculator
#   Identify the hand that you have from a set of cards.
#
# ===========================

source("Libraries.R")


# Hand Data Input ---------------------------------------------------------


# Imagine a set of 7 values {n1, n2, n3, n4, n5, n6, n7}
# Must pick the best subset of 5 by score.
# Data Format:
# {Number, Suit, Card, Value}
# {5, S, 5S, 4}

# Created by starting with a 2 card hand and adding cards from flop (3),
# river(1) and turn (1)

Hand <- tibble(Card = c("KS", "8S", "AS", "10S", "QS", "JS", "6C")) %>%
  left_join(., Deck)


# Hand Functions ----------------------------------------------------------

# Lets do a look up for patterns and see if we can convert them into functions.
# Q: Should we output that they pass? 
# A: Converted output to a list where the first factor is whether they pass or not.
# Q: Should we allow table output if test fails?
# A: For now I think we should just leave it as is to output whatever and let
# the first element be the determinant for whether to trust the table or not

# Royal flush : {AH, KH, QH, JH, 10H}

# CHeck Flush, then straight, then high card == A

RoyalFlush <- function(Hand){
  
  temp <- Hand
  
  A <- Flush(temp)
  if(nrow(A[[2]])==0){
    return(list(Pass = FALSE, A[[2]]))
  }
  B <- Straight(A[[2]]) #A[[2]] returns null when there is no flush. This fx doesn't work. an if fucntion catches that 
  C <- HighCard(A[[2]])
  
  x <- (A[[1]] && B[[1]]) && C[[2]]$Number == "A"
  y <- B[[2]]
  
  return(list(Pass = x, y))
}

# Straight flush

StraightFlush <- function(Hand){

temp <- Hand

A <- Flush(temp)
if(nrow(A[[2]])==0){
  return(list(Pass = FALSE, A[[2]]))
}
B <- Straight(A[[2]])

x <- A[[1]] && B[[1]]
y <- B[[2]]

return(list(Pass = x, y))
}
# Four of a kind
Fours <- function(Hand){

temp <- Hand
test <- c(table(temp$Value) == 4)
x <- reduce(test, `|`) # This statement must be true

temp$A <- test[match(temp$Value, names(test))]
y <- temp[temp$A, 1:4]

return(list(Pass = x, y))
}

# Full house
FullHouse <- function(Hand){

temp <- Hand
test1 <- c(table(temp$Value) == 3)
test2 <- c(table(temp$Value) == 2)

x <- reduce(test1, `|`) && reduce(test2, `|`) #This statement must be true or it is not FH

temp$A <- test1[match(temp$Value, names(test1))]
temp$B <- test2[match(temp$Value, names(test2))]

y <- temp[temp$A | temp$B, 1:4]

return(list(Pass = x, y))
}
# Flush
Flush <- function(Hand){
temp <- Hand
test <- c(table(temp$Suit) >= 5)
x <- reduce(test, `|`) #Must Pass

temp$A <- test[match(temp$Suit, names(test))]
temp <- temp[order(temp$Value),]

y <- slice_tail(temp[temp$A, 1:4],n=5) #Return Flush

return(list(Pass = x, y))
}
### Straight ###

Straight <- function(Hand){

# Unique Rule that A can act as a low card as well. The if statement appends low
# values Aces to the hand. We also need to select for the highest straight.


temp <- Hand
if("A" %in% temp$Number){
  temp <- add_row(temp, slice_max(temp, Value)[,1:3], Value = 0)
  }

temp <- temp[order(temp$Value),]

test <- rle(temp$Value - lag(temp$Value)) # counts run lengths
x <- reduce(test$lengths >= 4 & test$values == 1,`|`) #grabbing values of 1 (difference between n_2 and n_1) & 4 or more means that we have a straight

temp$A <- rep(test$lengths, test$lengths)
temp$B <- with(rle(temp$Value - lead(temp$Value)), rep(lengths, lengths))

y <- slice_tail(temp[temp$A >= 4 | temp$B >= 4,1:4],n = 5)

# Wow this was hard...
return(list(Pass = x, y))
}

# Three of a kind
Trips <- function(Hand){

temp <- Hand
test <- c(table(temp$Value) >= 3)
x <- reduce(test, `|`) #Must Pass

temp$A <- test[match(temp$Value, names(test))]
y <- temp[temp$A, 1:4] #Return Three of a kind

return(list(Pass = x, y))
}
# TWo pair
TwoPair <- function(Hand){
temp <- Hand
test <- c(table(temp$Value) >= 2)

x <- reduce(test, `|`) && sum(test) == 2 #Must Pass test #Must pass count

temp$A <- test[match(temp$Value, names(test))]
y <- temp[temp$A, 1:4] #Return Two Pair

return(list(Pass = x, y))
}
# One pair

OnePair <- function(Hand){

temp <- Hand
test <- c(table(temp$Value) >= 2)

reduce(test, `|`) #Must Pass test
x <- sum(test) == 1 #Must Pass count

temp$A <- test[match(temp$Value, names(test))]
y <- temp[temp$A, 1:4] #Return Pair

return(list(Pass = x, y))
}

# High Card

HighCard <- function(Hand){

temp <- Hand

x <- sum(!is.na(match(Hand$Value, max(Hand$Value)))) == 1 #Must Pass (though questionable)

y <- temp[!is.na(match(Hand$Value, max(Hand$Value))),] #Return Highcard

return(list(Pass = x,y))
}


# Wrapper Functions --------------------------------------------------------

FindHand <- function(Hand){
  
  if(nrow(Hand) == 0){
      list("Empty Hand")
    }
  else{
    case_when(
      RoyalFlush(Hand)[[1]] ~ list("Royal Flush", RoyalFlush(Hand)[[2]]),
      StraightFlush(Hand)[[1]] ~ list("Straight Flush", StraightFlush(Hand)[[2]]),
      Fours(Hand)[[1]] ~ list("Four of a Kind", Fours(Hand)[[2]]),
      FullHouse(Hand)[[1]] ~ list("Full House",  FullHouse(Hand)[[2]]),
      Flush(Hand)[[1]] ~ list("Flush", Flush(Hand)[[2]]),
      Straight(Hand)[[1]] ~ list("Straight",  Straight(Hand)[[2]]),
      Trips(Hand)[[1]] ~ list("Three of a Kind", Trips(Hand)[[2]]),
      TwoPair(Hand)[[1]] ~ list("Two Pair", TwoPair(Hand)[[2]]),
      OnePair(Hand)[[1]] ~ list("One Pair", OnePair(Hand)[[2]]),
      HighCard(Hand)[[1]] ~ list("High Card" , HighCard(Hand)[[2]])
    )
  }
}
