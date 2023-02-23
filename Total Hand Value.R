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
source("Hand Select.R")

# Hand Data Input ---------------------------------------------------------



# From the Hand Rank
# 
# Data Format:
# {Number, Suit, Card, Value}
# {5, S, 5S, 4}
# 
# Hand <- tibble(Card = c("JS", "10C", "10D" , "6S", "AH" , "7S", "8S")) %>%
#   left_join(., Deck)


# Constants ---------------------------------------------------------------


hand_rank <- 13
flush_bonus <- 1000


# Total Value Functions ---------------------------------------------------



TotalValue <- function(Hand){
  temp <- Hand
  a <- FindHand(temp)
  
HandVal <-  if( a[[1]] == "Royal Flush" || a[[1]] == "Straight Flush"){
    sum(a[[2]]$Value) + hand_rank^6 + flush_bonus
  }else if(a[[1]] == "Four of a Kind"){
    sum(a[[2]]$Value) + hand_rank^6
  } else if (a[[1]] == "Full House") {
    sum(a[[2]]$Value[a[[2]]$Value == names(table(a[[2]]$Value)[table(a[[2]]$Value) ==
                                                                 2])],
        13 * a[[2]]$Value[a[[2]]$Value == names(table(a[[2]]$Value)[table(a[[2]]$Value) ==
                                                                      3])]) + hand_rank^5
  }else if(a[[1]] == "Flush"){
    sum(a[[2]]$Value * c(13^2,13,1,13^-1,13^-2),hand_rank^4,flush_bonus)
  }else if(a[[1]] == "Straight"){
    sum(a[[2]]$Value) + hand_rank^4
  }else if(a[[1]] == "Three of a Kind"){
    sum(a[[2]]$Value) + hand_rank^3
  }else if(a[[1]] == "Two Pair"){
    sum(3*max(a[[2]]$Value), 0.25*min(a[[2]]$Value), hand_rank^2)
  }else if(a[[1]] == "One Pair"){
    sum(2*a[[2]]$Value,13)
  }else{
    a[[2]]$Value
  }

# HandVal <-  case_when( a[[1]] == "Royal Flush" || a[[1]] == "Straight Flush" ~
#   sum(a[[2]]$Value) + hand_rank^6 + flush_bonus, 
#   a[[1]] == "Four of a Kind" ~ sum(a[[2]]$Value) + hand_rank^6,
#   a[[1]] == "Full House" ~
#   sum(a[[2]]$Value[a[[2]]$Value == names(table(a[[2]]$Value)[table(a[[2]]$Value) ==
#                                                                  2])],
#         13 * a[[2]]$Value[a[[2]]$Value == names(table(a[[2]]$Value)[table(a[[2]]$Value) ==
#                                                                       3])]) + hand_rank^5,
#   a[[1]] == "Flush" ~ sum(a[[2]]$Value * c(13^2,13,1,13^-1,13^-2),hand_rank^4,flush_bonus),
#   a[[1]] == "Straight" ~ sum(a[[2]]$Value) + hand_rank^4,
#   a[[1]] == "Three of a Kind" ~ sum(a[[2]]$Value) + hand_rank^3,
#   a[[1]] == "Two Pair" ~ sum(3*max(a[[2]]$Value), 0.25*min(a[[2]]$Value), hand_rank^2),
#   a[[1]] == "One Pair" ~ sum(2*a[[2]]$Value,13),
#   TRUE ~ a[[2]]$Value[1]
# )
#   
  
b <-
  anti_join(temp, a[[2]], by = c("Card", "Number", "Suit", "Value")) %>% 
  arrange(., desc(Value))
b <- b[1:(5-nrow(a[[2]])), ]

Residuals <- sum(b$Value/((13)^(row_number(b$Value))))

ab <- rbind(a[[2]],b)
 
  return(list(HandVal + Residuals, ab))
  
}



