library(tidyverse)
library(purrr)

## Input
test <- matrix(c(1, 1,
                 1, 6,
                 8, 3,
                 3, 4,
                 5, 5,
                 8, 9), 
               ncol =2, byrow = TRUE) %>% 
  as_tibble() %>% set_names(c("y", "x"))

input <- read_csv("input/day6.txt", col_names = c("y", "x"))

## find clostest neighbor(s) of (i,j) within centers
find_closest <- function(i, j, centers) {
  centers %>% transmute(dist = abs(x -i) + abs(y - j)) %>% pull %>% { d <- min(.); which(. == d) }
}

total_distance <- function(i, j, centers) {
  centers %>% summarise(dist = sum(abs(x -i)) + sum(abs(y - j))) %>% pull
}

## Create a rectangle that contains all centers and extend it by 1 unit in all directions
## Flag each according to whether or not they are in the extension
create_area <- function(input, margin = 1) { 
  x.range <- input %>% pull(x) %>% range
  y.range <- input %>% pull(y) %>% range
  in_interval <- function(x, range) { x <= range[2] & x >= range[1] }
  ## create test grid and find which center is closest to each node
  crossing(i = (x.range + c(-margin, margin)) %>% {seq.int(from = .[1], to = .[2])}, 
           j = (x.range + c(-margin, margin)) %>% {seq.int(from = .[1], to = .[2])}) %>% 
    mutate(cvx.hull = in_interval(i, x.range) & in_interval(j, y.range))
}
 
## Find area of the voronoi cell of each center
find_areas <- function(input) {
  ## Find which center is closest to each node and remove nodes with two or closest neighbors
  test.area <- create_area(input) %>% 
    mutate(closest.center   = map2(i, j, find_closest, centers = input), 
           nb.closest       = map_int(closest.center, length)) %>% 
    filter(nb.closest == 1) %>% 
    unnest 
  ## Compute the area of the "voronoi cell" of each center both within 
  ## the "convex hull" and in a slightly extended version (to find which cells grow to infinity)
  test.area %>% group_by(closest.center) %>% 
    summarize(area         = sum(cvx.hull), 
              extended.area = n())
}

## compute total distance
compute_total_distance <- function(input, margin = 1) {
  test.area <- create_area(input, margin = 1) %>% 
    mutate(total.distance = map2_dbl(i, j, total_distance, centers = input))
  expand <- test.area %>% filter(!cvx.hull, total.distance < 10000) %>% nrow()
  if (expand) {
    warning("You may have missed some nodes with total distance lower thant 10000. Consider expanding the margin")
  }
  test.area
}

solve_problem_1 <- function(input) {
  find_areas(input) %>% 
    filter(area == extended.area) %>% 
    pull(area) %>% 
    max()
}

solve_problem_2 <- function(input, threshold) {
  compute_total_distance(input) %>% 
    filter(total.distance < threshold) %>% 
    nrow()
}

## Try pure tidyverse solutions
solve_problem_1_bis <- function(input) {
  create_area(input) %>% 
    mutate(centers = list(input)) %>% 
    unnest(centers) %>%
    mutate(dist = abs(i - x) + abs(j - y)) %>%
    group_by(i, j) %>%
    summarise(cvx.hull       = cvx.hull[1], 
              min.dist       = min(dist), 
              n.neighbors    = sum(dist == min.dist), 
              closest.center = which.min(dist)) %>%
    filter(n.neighbors == 1) %>%
    group_by(closest.center, add = FALSE) %>% 
    summarize(area         = sum(cvx.hull), 
              extended.area = n()) %>% 
    filter(area == extended.area) %>% 
    pull(area) %>% 
    max()
}


solve_problem_2_bis <- function(input, threshold) {
  create_area(input) %>% 
    mutate(centers = list(input)) %>% 
    unnest(centers) %>%
    mutate(dist = abs(i - x) + abs(j - y)) %>%
    group_by(i, j) %>%
    summarise(tot_dist = sum(dist)) %>%
    filter(tot_dist < threshold) %>%
    nrow()
}

solve_problem_1(test) ## 0.26s
solve_problem_1_bis(test) ## 0.21s
solve_problem_1(input) ## 184s
system.time(solve_problem_1_bis(input)) ## 184s

solve_problem_2(test, 32) ## 0.079s
solve_problem_2_bis(test, 32) ## 0.025s
solve_problem_2(input, 10000) # 71s
solve_problem_2_bis(input, 10000) # 3s
