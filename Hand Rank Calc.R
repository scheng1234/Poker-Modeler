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
source("Hand Calc Support.Rmd")


# Hand Data Input ---------------------------------------------------------



# Imagine a set of 7 values {n1, n2, n3, n4, n5, n6, n7}
# Must pick the best subset of 5 by score.
# Data Format:
# {Number, Suit, Card, Value}
# {5, S, 5S, 4}

# Created by starting with a 2 card hand and adding cards from flop (3), river(1) and turn (1)

Hand <- tibble(Card = c("2S", "5S", "3S" , "4S", "AS" , "QH", "6H")) %>% 
  left_join(., Deck)




# Extra Functions ---------------------------------------------------------



Straight <- function(){}
Flush <- function(){}



# Hand Functions ----------------------------------------------------------

# Lets do a look up for patterns and see if we can functionalize those patterns from the hands that we're given

# Royal flush : {AH, KH, QH, JH, 10H}

test <- table(Hand$Suit)
test >= 5

Hand$Value

# Straight flush
# Four of a kind
# Full house


# Flush
temp <- Hand
test <- c(table(temp$Suit) >= 5)
reduce(test, or) #Must Pass

temp$A <- test[match(temp$Suit, names(test))]
temp[temp$A, 1:4] #Return Flush

### Straight

temp <- Hand
temp <- temp[order(temp$Value),]

temp$Value - temp$Value



# Three of a kind
temp <- Hand
test <- c(table(temp$Value) >= 3)
reduce(test, or) #Must Pass

temp$A <- test[match(temp$Value, names(test))]
temp[temp$A, 1:4] #Return Three of a kind


# TWo pair

temp <- Hand
test <- c(table(temp$Value) >= 2)

reduce(test, or) #Must Pass test
sum(test) == 2 #Must pass count

temp$A <- test[match(temp$Value, names(test))]
temp[temp$A, 1:4] #Return Two Pair

# One pair
temp <- Hand
test <- c(table(temp$Value) >= 2)

reduce(test, or) #Must Pass test
sum(test) == 1 #Must Pass count

temp$A <- test[match(temp$Value, names(test))]
temp[temp$A, 1:4] #Return Pair

# High Card
temp <- Hand

sum(!is.na(match(Hand$Value, max(Hand$Value)))) == 1 #Must Pass (though questionable)

temp[!is.na(match(Hand$Value, max(Hand$Value))),] #Return Highcard
