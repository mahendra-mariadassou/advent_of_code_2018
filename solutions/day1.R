library(tidyverse)

## Day 1
read_lines("input/day1.txt") %>% 
  as.numeric %>% 
  sum()

## Day 1 - variant - use hash tables for quick lookup of existing values
## Could be done faster with a binary search tree
input <- read_lines("input/day1.txt") %>% as.numeric()

detect_duplicate <- function(input) {
  
  position <- 1
  n <- length(input)
  ## intialize hash table 
  hash <- new.env(hash = TRUE, emptyenv())
  
  x <- input[1]
  while (!exists(as.character(x), hash)) {     ## test existence in hash table 
    assign(as.character(x), NULL, hash)        ## if not it and update x and position
    position <- position + 1
    x <- x + input[(position - 1) %% n + 1]
  }
  return(list(x, position))
}

detect_duplicate(input)