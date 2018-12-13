library(tidyverse)

cell_power <- function(x, y, serial.number = 8) {
  hundred_number <- function(x) { x %/% 100 %% 10}
  rack.ID <- x + 10
  ((rack.ID * y + serial.number) * rack.ID) %>% hundred_number %>% `-`(5)
}

grid_power <- function(grid.size = 5, serial.number = 8) {
  grid <- outer(1:grid.size, 1:grid.size, cell_power, serial.number = serial.number)
  subgrid_power <- function(i, j) {
    sum(grid[i + 0:2, j + 0:2])
  }
  crossing(x = 1:(grid.size - 2), y = 1:(grid.size - 2)) %>% 
    group_by(x, y) %>% 
    mutate(power = subgrid_power(x, y)) %>% 
    ungroup()
}

## checks
## cell_power(3, 5, 8) ## 4
## cell_power(122, 79, 57) ## -5
## cell_power(217, 196, 39) ## 0
## cell_power(101, 153, 71) ## 4

solve_problem_1 <- function(serial.number) {
  grid_power(grid.size = 300, serial.number = serial.number) %>% 
    arrange(desc(power)) %>% 
    slice(1) %>% 
    select(x, y) %>% 
    unite(col = "xy", sep = ",") %>% 
    pull()
}

## checks
# solve_problem_1(18) ## 1.7s 33,45
# solve_problem_1(42) ## 1.7s 21,61
solve_problem_1(2694) ## 1.7s 243,38

preprocess <- function(x) {
  ## compute partial sums: X[i, j] = sum(x[i:nrow(x), j:ncol(x)])
  # sums <- matrix(NA, nrow(x), ncol(x))
  # ## Initialize sums border
  # sums[nrow(x), ] <- cumsum(x[nrow(x), ] %>% rev()) %>% rev()
  # sums[, ncol(x)] <- cumsum(x[, ncol(x)] %>% rev()) %>% rev()
  # for (i in (nrow(x)-1):1) {
  #   for (j in (ncol(x)-1):1) {
  #     sums[i, j] <- sums[i, j + 1] + sums[i + 1, j] - sums[i+1, j+1] + x[i, j]
  #   }
  # }
  ## Faster versions: reverse x, accumulate by row, accumulate by col and reverse again
  reverse_matrix <- function(x) { x %>% rev() %>% `dim<-`(c(ncol(x), nrow(x))) }
  x %>% reverse_matrix() %>% apply(1, cumsum) %>% t() %>% apply(2, cumsum) %>% reverse_matrix()
}

solve_problem_2 <- function(grid.size = 300, serial.number = 18) {
  grid <- outer(1:grid.size, 1:grid.size, cell_power, serial.number = serial.number)
  partial.sums <- preprocess(grid)
  ## pad with an extra line and column of 0s to account for empty sums 
  partial.sums <- partial.sums %>% rbind(0) %>% cbind(0)
  query <- function(x, y, size) {
    ## sum of elements in grid[x:(x+size), y:(y+size)], vectorized over x, y and size
    ## vectorized version of partial.sums[x,y] - partial.sums[x+size,y] - partial.sums[x,y+size] + partial.sums[x+size, y+size]
    ## sum of elements in partial.sums[x:(x+size), y:(y+size)]
    index <- (y - 1) * (grid.size + 1) + x
    shift <- size * (grid.size + 1)
    partial.sums[index] - partial.sums[index + size] - partial.sums[index + shift] + partial.sums[index + shift + size]
  }
  crossing(x = 1:grid.size, y = 1:grid.size, size = 1:grid.size) %>% ## all subsquares 
    filter(pmax(x, y) + size <= grid.size + 1) %>%                   ## only valid ones
    mutate(sum = query(x, y, size)) %>% 
    top_n(1, sum)
}

## solve_problem_2(300, 18) ## 3.2, 90,269,16
solve_problem_2(300, 2694) ## 3.2s, 235,146,13
