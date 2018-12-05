library(tidyverse)

## Input
test <- c("#1 @ 1,3: 4x4",
          "#2 @ 3,1: 4x4",
          "#3 @ 5,5: 2x2")
input <- read_lines("input/day3.txt")

format_input <- function(x) {
  x %>%
    str_remove_all(" ") %>%
    str_remove_all("#") %>%
    str_split(pattern = '@|,|:|x', n = Inf) %>%
    do.call(rbind, args = .) %>%
    as_tibble() %>%
    mutate_all(as.numeric) %>%
    # x,y coordinates of top-left corner with top-left corner at position (1, 1)
    set_names(nm = c("claim", "x", "y", "width", "height"))
}

fabric <- function(x, data = NULL) {
  ## format claims in tidy format
  if (is.null(data)) {
    data <- format_input(x)
  }
  ## Create a piece of fabric
  fabric <- matrix(
    data = 0,
    ncol = with(data, max(x + width)),
    nrow = with(data, max(y + height))
  )
  data %>%
    select(x:height) %>%
    pwalk(function(x, y, width, height) {
      fabric[x - 1 + 1:width, y - 1 + 1:height] <<- fabric[x - 1 + 1:width, y - 1 + 1:height] + 1
    })
  fabric
}

## Problem 1

count_conflicts <- function(x) {
  sum(fabric(x) > 1)
}

count_conflicts(test) ## 4
count_conflicts(input) ## 110891

## Problem 2
find_non_overlapping <- function(x) {
  data <- format_input(x)
  fabric <- fabric(x, data)
  data %>% 
    mutate(overlapping = pmap_lgl(., function(claim, x, y, width, height) {
      any(fabric[x - 1 + 1:width, y - 1 + 1:height] > 1)
    })) %>% 
    filter(!overlapping)
}

find_non_overlapping(test)
find_non_overlapping(input) ## 0.045s, claim 297
