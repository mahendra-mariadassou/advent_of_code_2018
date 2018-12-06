library(tidyverse)

## Input
test <- "dabAcCaCBAcCcaDA"
input <- read_lines("input/day5.txt")

pattern <- c(str_c(letters, LETTERS), 
             str_c(LETTERS, letters)) %>% 
  str_flatten(collapse = "|")

## Problem 1
chain_reaction <- function(input) {
  while (str_detect(input, pattern)) {
    input <- str_remove_all(input, pattern)
  }
  input
}

solve_problem_1 <- function(input) {
  input %>% chain_reaction %>% nchar
}

solve_problem_1(test)

solve_problem_1(input)

## Problem 2
solve_problem_2 <- function(input) {
  input <- chain_reaction(input)
  tibble(letter = str_c(letters, "|", LETTERS)) %>% 
    mutate(subpoly = str_remove_all(input, pattern = letter), 
           endpoly = map_chr(subpoly, chain_reaction), 
           size    = nchar(endpoly))
}
  
solve_problem_2(test) %>% arrange(size) %>% slice(1) %>% pull(size) ## 4
  
solve_problem_2(input) %>% arrange(size) %>% slice(1) %>% pull(size) ## 21.7s, 6336
