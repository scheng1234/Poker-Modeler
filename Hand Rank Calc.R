# ===========================
#   
#   Author: Steven Cheng
#   Date: 11/14/2022
#   
#   Hand Calculator
#   Calculate the value of Hands possible
#
# ===========================

source("Libraries.R")


# Hand Data Input ---------------------------------------------------------



# Imagine a set of 7 values {n1, n2, n3, n4, n5, n6, n7}
# Must pick the best subset of 5 by score.
# Data Format:
# {Number, Suit, Card, Value}
# {5, S, 5S, 4}

# Created by starting with a 2 card hand and adding cards from flop (3), river(1) and turn (1)

Hand <- tibble(Card = c("AS", "QS", "8S" , "JS", "10S" , "9S", "8H")) %>% 
  left_join(., Deck)




# Extra Functions ---------------------------------------------------------



Straight <- function(){x=5}
Flush <- function(g){ 
  testfunc <- function(y){x=6
  return(x)
  }
}



# Hand Functions ----------------------------------------------------------

# Lets do a look up for patterns and see if we can functionalize those patterns from the hands that we're given

# Royal flush : {AH, KH, QH, JH, 10H}

# CHeck Flush, then straight, then high card == A

# Straight flush

# Check Flush, then Straight



# Four of a kind
temp <- Hand
test <- c(table(temp$Value) == 4)
reduce(test, `|`) # This statement must be true

temp$A <- test[match(temp$Value, names(test))]
temp[temp$A, 1:4]

# Full house
temp <- Hand
test1 <- c(table(temp$Value) == 3)
test2 <- c(table(temp$Value) == 2)

reduce(test1, `|`) && reduce(test2, `|`) #This statement must be true or it is not FH

temp$A <- test1[match(temp$Value, names(test))]
temp$B <- test2[match(temp$Value, names(test))]

temp[temp$A | temp$B, 1:4]


# Flush
temp <- Hand
test <- c(table(temp$Suit) >= 5)
reduce(test, or) #Must Pass

temp$A <- test[match(temp$Suit, names(test))]
temp[temp$A, 1:4] #Return Flush

### Straight ###

# Unique Rule that A can act as a low card as well. The if statement appends low values Aces to the hand. We also need to select for the highest straight.


temp <- Hand
if("A" %in% temp$Number){
  temp <- add_row(temp, slice_max(temp, Value)[,1:3], Value = 0)
  }

temp <- temp[order(temp$Value),]

test <- rle(temp$Value - lag(temp$Value)) # counts run lengths
reduce(test$lengths >= 4 & test$values == 1,`|`) #grabbing values of 1 (difference between n_2 and n_1) & 4 or more means that we have a straight

with(rle(temp$Value - lag(temp$Value)), rep(lengths, lengths)) # This somehow works but not sure why

temp$A <- rep(test$lengths, test$lengths)
temp$B <- with(rle(temp$Value - lead(temp$Value)), rep(lengths, lengths))

slice_tail(temp[temp$A >= 4 | temp$B >= 4,1:4],n = 5)

# Wow this was hard...

# Three of a kind
temp <- Hand
test <- c(table(temp$Value) >= 3)
reduce(test, `|`) #Must Pass

temp$A <- test[match(temp$Value, names(test))]
temp[temp$A, 1:4] #Return Three of a kind


# TWo pair

temp <- Hand
test <- c(table(temp$Value) >= 2)

reduce(test, `|`) #Must Pass test
sum(test) == 2 #Must pass count

temp$A <- test[match(temp$Value, names(test))]
temp[temp$A, 1:4] #Return Two Pair

# One pair
temp <- Hand
test <- c(table(temp$Value) >= 2)

reduce(test, `|`) #Must Pass test
sum(test) == 1 #Must Pass count

temp$A <- test[match(temp$Value, names(test))]
temp[temp$A, 1:4] #Return Pair

# High Card
temp <- Hand

sum(!is.na(match(Hand$Value, max(Hand$Value)))) == 1 #Must Pass (though questionable)

temp[!is.na(match(Hand$Value, max(Hand$Value))),] #Return Highcard
