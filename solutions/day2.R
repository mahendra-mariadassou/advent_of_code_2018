library(tidyverse)

## Day 2

input <- read_lines("input/day2.txt")

checksum <- function(box.ids) {
  tibble(ID = box.ids) %>% 
    mutate(df_count = map(ID, ~ tibble(letter = letters, count = str_count(., pattern = letters)))) %>% 
    unnest(df_count) %>% 
    filter(count > 1, count < 4) %>% 
    distinct(ID, count) %>% 
    group_by(count) %>% 
    summarise(nb_IDs = n()) %>% 
    pull(nb_IDs) %>% 
    prod()
}

system.time(checksum(input))

## Day 2 - variant
test <- c("abcde",
          "fghij",
          "klmno",
          "pqrst",
          "fguij",
          "axcye",
          "wvxyz")

find_duplicate <- function(input) {
  n <- length(input)
  crossing(i = 1:n, j = 1:n) %>% 
    filter(i < j) %>% 
    mutate(ID = input[i], 
           ID2 = input[j]) %>% 
    mutate(ID_split = map(ID, ~ str_split_fixed(., "", Inf)), 
           ID2_split = map(ID2, ~ str_split_fixed(., "", Inf))) %>%
    mutate(diff   = map2_int(ID_split, ID2_split, ~ sum(.x != .y))) %>% 
    filter(diff == 1) %>% 
    mutate(subset = map2_chr(ID_split, ID2_split, ~ .x[.x == .y] %>% str_flatten)) %>% 
    pull(subset)
}

system.time(find_duplicate(input))

## Day 2 - variant inspired by @abichat (and much more efficient)

find_duplicate_2 <- function(input) {
  N <- str_count(input[1])
  tibble(position = 1:N) %>% 
    mutate(sub_words = map(position, ~ str_c(str_sub(input, 0, . - 1), str_sub(input, . + 1, N)))) %>% 
    unnest(sub_words) %>% 
    group_by(position, sub_words) %>%
    summarise(N = n()) %>%
    filter(N == 2) %>% 
    pull(sub_words)
}

system.time(find_duplicate_2(input))
