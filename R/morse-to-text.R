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

## ---- morseToLatin ----
# function that translates one letter of morse to latin alphabet 
# (incl. gaps between letters & spaces between words)
morseToLatin <- function(m){
  latin <- paste0(filter(dictionary, morseCode == m)[, "latinAlphabet"])
  return(latin)
}

## ---- morseCodeToText ----
# function that takes in a string of morsecode and translates it to latin alphabet
# using the above defined dictionary and the morseToLatin function
morseCodeToText <- function(morsecodestr) {
  # turn morse code into one string (if not given as one string)
  morsecodestr <- paste0(morsecodestr, collapse="")
  
  # split into words if seven zeros are preceded by 1 OR followed by 1
  mwords <-
    unlist(strsplit(morsecodestr, "(?<=1)(?=0000000)|(?<=0000000)(?=1)", 
                    perl = TRUE))
  
  # split words into letters if three zeros are preceded by 1 OR followed by 1
  mletters <-
    unlist(strsplit(mwords, "(?<=1)(?=000)|(?<=000)(?=1)", perl = TRUE))
  
  # apply the above function morseToLatin over a vector of morse code elements
  out = paste0(sapply(mletters, morseToLatin, USE.NAMES = FALSE), 
               collapse = "")
  
  return(out)
}

## ---- test-examples ----
# test with example morsecode 
morsecode <- "10101010001000000010111010100010111010100011101110111"
morseCodeToText(morsecode)
morsecode1 <- "1010101000100010111010100010111010100011101110111"
morseCodeToText(morsecode1)

morsecode2 <- c("1010101000100010111010100010111010100011101110111", "0000000", "101110111")
morseCodeToText(morsecode2)
morsecode3 <- c("1", "0", "1", "0", "1", "0", "1", "000", "1", "0", "111", "000", "1", "0", "1") 
morseCodeToText(morsecode3)
