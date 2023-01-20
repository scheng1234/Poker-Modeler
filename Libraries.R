

library(tidyverse)
library(robotstxt)
library(rvest)
library(selectr)
library(xml2)
library(magrittr)
library(lubridate)
library("furrr")


# Optionals and Notes -----------------------------------------------------


library(holdem) # Might be helpful for random dealing & game simulation since i'm not interested in creating these functions.
                # Will have to translate to my coding of cards.
    # count1() is the same as rle()
    # deal1() appears to be possible to deal >5 of the same value cards