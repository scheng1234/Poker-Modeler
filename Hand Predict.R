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

set.seed(18610)
Hand <- DealFromHoldem()[[1]]


# Maximal Hand Value ------------------------------------------------------

# We will need to determine whether we are on the "pre-flop, flop, or turn"


# Pre Flop
Hand[1:2,]

# Flop
Hand[1:5,]

# Turn
Hand[1:6,]

# Expected Hand value -----------------------------------------------------

# We will want to calculate the expected hand strength given "flop, river, or turn"
