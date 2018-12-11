library(tidyverse) 

test <- c(n.players = 9, n.marbles = 26)

play_game <- function(n.players = 9, n.marbles = 26) {
  ## Book keeping: left and right act as a doubled chained list
  left  <- rep(NA, n.marbles) ## counter-clockwise neigbhor
  right <- rep(NA, n.marbles) ## clockwise neighbor 
  player <- 1                 ## current player
  marble <- 1                 ## next marble
  current <- marble           ## current marble
  gains <- rep(0, n.players)  ## gains of different players
  ## helper functions
  tsugi <- function(current, n.steps = 1) { 
    ## find marbles n.steps away from current marble in clockwise order
    ## can't use next, use "tsugi" instead
    if (n.steps == 0) return(current)
    tsugi(right[current], n.steps - 1)
  }
  previous <- function(current, n.steps = 1) { 
    ## find marbles n.steps away from current marble in counter-clockwise order
    if (n.steps == 0) return(current)
    previous(left[current], n.steps - 1)
  }
  ## insert target marble after (in clockwise direction) current marble
  insert_after <- function(current, target) {
    left.marble <- current; right.marble <- tsugi(current)
    left[target] <<- left.marble; right[target] <<- right.marble
    right[left.marble] <<- target; left[right.marble] <<- target  
  }
  ## remove current marble from the circle
  remove_marble <- function(current) {
    prev.marble <- previous(current); next.marble <- tsugi(current)
    left[next.marble] <<- prev.marble
    right[prev.marble] <<- next.marble
  }
  ## normal move
  normal_move <- function() {
    insert_after(tsugi(current), marble)
    current <<- marble 
  }
  special_move <- function() {
    marble.2 <- previous(current, n.steps = 7)
    gains[player] <<- gains[player] + (marble-1) + (marble.2 - 1)
    remove_marble(marble.2)
    current <<- tsugi(marble.2)
  }
  move <- function() {
   if (marble %% 23 == 1) {
     special_move()
   } else {
     normal_move()
   }
    player <<- (player %% n.players) + 1
    marble <<- marble + 1
  }
  print_state <- function() {
    print(glue::glue("Round {marble-1}, Player {player}"))
    cur.marble <- 1
    x <- c(cur.marble)
    cur.marble <- tsugi(cur.marble)
    while (cur.marble != 1) {
      x <- c(x, cur.marble)
      cur.marble <- tsugi(cur.marble)
    }
    ## return(x)
    print(paste0("Circle: ", str_c(x-1, collapse = " ")))
  }
  ## Initiate circle 
  left[1] <- right[1] <- marble;
  marble <- marble + 1
  ## Play game 
  while (marble <= n.marbles) {
    move()
  }
  return(gains)
}

play_game()


solve_problem_1 <- function(n.players, n.marbles) {
 play_game(n.players, n.marbles) %>% max() 
}

# solve_problem_1(n.players = 9, n.marbles = 26))    ## 32
# solve_problem_1(n.players = 10, n.marbles = 1619)  ## 8317
# solve_problem_1(n.players = 13, n.marbles = 8000)  ## 146373
# solve_problem_1(n.players = 17, n.marbles = 1105)  ## 2764
# solve_problem_1(n.players = 21, n.marbles = 6112)  ## 54718
# solve_problem_1(n.players = 30, n.marbles = 5808)  ## 37305

## Problem 1
solve_problem_1(n.players = 476, n.marbles = 71432)  ## 0.307s, 384205

## Problem 2
solve_problem_1(n.players = 476, n.marbles = 71431*100 + 1)  ## 31s, 384205
