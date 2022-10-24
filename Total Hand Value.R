# ===========================
#   
#   Author: Steven Cheng
#   Date: 10/18/2022
#   
#   Total Value
#   Calculate the Total Value of Hands
#
# ===========================

source("Libraries.R")


# Hand Data Input ---------------------------------------------------------



# From the Hand Rank
# 
# Data Format:
# {Number, Suit, Card, Value}
# {5, S, 5S, 4}

Hand <- tibble(Card = c("AS", "QS", "8S" , "JS", "10S" , "9S", "8H")) %>%
  left_join(., Deck)

