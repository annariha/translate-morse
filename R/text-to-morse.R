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

textToMorsecode <- function(latinStr) {
  # input text should be tokenized

  # apply the above function latinToMorse to latin letters+spaces
  out = sapply(latinStr, latinToMorse, USE.NAMES = FALSE)
  
  return(out)
}

# TODO: insert "" to text vector
# function modified and taken from: https://stackoverflow.com/questions/18951248/insert-elements-in-a-vector-in-r

insert.gap.at <- function(a, pos){
  # include a gap 
  include <- c("")
  result <- vector("list",2*length(pos)+1)
  result[c(TRUE,FALSE)] <- split(a, cumsum(seq_along(a) %in% (pos+1)))
  result[c(FALSE,TRUE)] <- include
  unlist(result)
}

text = c("h", "e", " ", "y", "o")
positions = c(1,4)

insert.at(text, positions)
# This works! 

# Now, I need to define the positions automatically.
# I have the tokenized text vector and 
# I want a positions-vector with every position 
# that needs to be followed by a ""-like gap.
# These are all the positions that are NOT preceded or followed by a " "-space.

# example input 
gutenberg_works(title == "The Time Machine")
hgwellstm <- gutenberg_download(35)
text <- strsplit(tolower(paste0(hgwellstm$text,collapse="")),"")[[1]]
example_text <- text[1:16]
