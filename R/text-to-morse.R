# load packages
library(tictoc)
library(gutenbergr)
library(dplyr)

# create dictionary
myletters <- c(letters, "", " ")

morsealphabet <- c("10111", "111010101", "11101011101", "1110101",
                   "1", "101011101", "111011101", "1010101", "101",
                   "1011101110111", "111010111", "101110101",
                   "1110111", "11101", "11101110111", "10111011101",
                   "1110111010111", "1011101", "10101", "111",
                   "1010111", "101010111", "101110111", "11101010111",
                   "1110101110111", "11101110101", "000", "0000000")

dictionary <- data.frame(cbind(latinAlphabet = myletters, morseCode = morsealphabet))

## ---- latinToMorse ----
# function that translates one latin letter to morsecode (including "" and " ")
latinToMorse <- function(l){
  morse <- paste0(filter(dictionary, latinAlphabet == l)[, "morseCode"])
  return(morse)
}

## ---- textToMorsecode ----
# function that takes in a string in latin alphabet and translates it to morsecode 
# using the above defined dictionary and the latinToMorse-function

textToMorsecode <- function(latinstr) {
  # input text should be tolower 
  latinstr <- tolower(latinstr)
  # filter for letters and symbols in latinalphabet (created for the dictionary)
  latinstr <- latinstr[which(latinstr %in% latinalphabet)]
  # apply the above function latinToMorse to latin letters+spaces
  out = sapply(latinstr, latinToMorse, USE.NAMES = FALSE)
  return(out)
}