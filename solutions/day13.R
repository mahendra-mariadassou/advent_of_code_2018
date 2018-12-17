library(tidyverse)

test <- read_lines(file = "input/day13_test.txt")
test2 <- read_lines(file = "input/day13_test2.txt")
input <- read_lines(file = "input/day13.txt")


format_input <- function(input) {
  ## x denotes columns, y denotes rows
  input %>% str_split_fixed(pattern = "", n = Inf) %>% 
    reshape2::melt(value.name = "segment", varnames = c("y", "x")) %>% 
    mutate(segment = as.character(segment)) %>% 
    filter(segment != " ") %>% 
    as.tibble()
}

build_track <- function(x) {
  x %>% 
    mutate(segment = segment %>% str_replace("v|\\^", "|") %>% str_replace("<|>", "-")) %>% 
    mutate(direction = case_when(segment == "|"  ~ "vertical", 
                                 segment == "-"  ~ "horizontal", 
                                 segment == "+"  ~ "intersection",
                                 segment == "/"  ~ "top.left.corner",      ## could have bottom right
                                 segment == "\\" ~ "top.right.corner"      ## could have bottom left
                                 ))
}

build_track_base <- function(input) {
  dictionary <- c("|"  = "vertical", 
                  "-"  = "horizontal", 
                  "^"  = "vertical",
                  "v"  = "vertical",
                  "+"  = "intersection", 
                  "/"  = "top.left.corner", 
                  "\\" = "top.right.corner")
  input %>% str_split_fixed(pattern = "", n = Inf)
}

build_vehicles <- function(x) {
  speed <- tibble(segment = c("^", "v", "<", ">"), 
                  speed.x = c(0, 0, -1, 1), 
                  speed.y = c(-1, 1, 0, 0))
  x %>% 
    filter(segment %in% c("^", "v", "<", ">")) %>% 
    mutate(ID = row_number(), n.turns = 0) %>% 
    left_join(speed, by = "segment") %>% 
    select(-segment)
}

resolve_intersection <- function(direction, speed.x, speed.y, n.turns) {
  ## resolve intersection for each vehicle, turn relative instructions such as left turn, right turn and such 
  ## to absolute trajectory segments
  behavior <- if_else(direction != "intersection", 
                      direction, 
                      case_when(n.turns %% 3 == 1 ~ "left.turn", 
                                n.turns %% 3 == 2 ~ "straight", 
                                n.turns %% 3 == 0 ~ "right.turn"))
  case_when((behavior == "left.turn")  & (speed.x != 0) ~ "top.left.corner",   
            (behavior == "right.turn") & (speed.x != 0) ~ "top.right.corner", 
            (behavior == "left.turn")  & (speed.y != 0) ~ "top.right.corner", 
            (behavior == "right.turn") & (speed.y != 0) ~ "top.left.corner", 
            TRUE                                        ~ direction
  )
}

update_speed <- function(tbl) {
  ## Update speed based n current position, speed and number of turns
  tbl %>% 
    mutate(n.turns   = if_else(direction == "intersection", n.turns + 1, n.turns), 
           direction = resolve_intersection(direction, speed.x, speed.y, n.turns)) %>% 
    ## use simple composition rules to update speeds: speed (1, 0) + top.left.corner "/" = speed (0, -1)
    ## top.right.border exchanges speed.x and speed.y
    ## top.left.border inverts speed.x and speed.y before exchanging them
    mutate(corner      = str_detect(direction, "corner"), 
           new.speed.x = if_else(corner, speed.y, speed.x) * if_else(direction == "top.left.corner", -1, 1), 
           new.speed.y = if_else(corner, speed.x, speed.y) * if_else(direction == "top.left.corner", -1, 1)) %>% 
    mutate(speed.x = new.speed.x, 
           speed.y = new.speed.y) %>% 
    select(-new.speed.x, -new.speed.y, -corner)
}

update_positions <- function(tbl) {
  tbl %>% 
    mutate(old.x = x, 
           x = x + speed.x, 
           old.y = y, 
           y = y + speed.y)
}

print_state <- function(tracks, vehicles) {
  p <- left_join(tracks, vehicles, by = c("x", "y")) %>%
    mutate(vehicle = !is.na(ID), 
           segment = case_when(speed.x ==  1 ~ ">",
                               speed.x == -1 ~ "<", 
                               speed.y ==  1 ~ "v",
                               speed.y == -1 ~ "^",
                               TRUE          ~ segment)) %>% 
    ggplot(aes(x = x, y = -y, label = `segment`)) + 
    geom_tile() + 
    geom_text(color = "red", size = 10)
  plot(p)
}

play_game <- function(input) {
  x <- format_input(input) 
  ## tracks and vehicles
  tracks   <- build_track(x)
  vehicles <- build_vehicles(x)
  ## Book keeping
  ## 2D matrix for keeping track of collisions
  positions <- matrix(0, 
                      ncol = tracks %>% pull(x) %>% max, 
                      nrow = tracks %>% pull(y) %>% max)
  positions[cbind(vehicles %>% pull(y), vehicles %>% pull(x))] <- 1
  crash <- FALSE
  epochs <- 0
  one_move <- function() {
    ## Order carts based on current location before updating location
    vehicles <<- vehicles %>% select(ID, n.turns, x, y, speed.x, speed.y) %>% 
      arrange(y, x) %>% update_positions()
  }
  check_crash <- function() {
    for (i in 1:nrow(vehicles)) {
      old.pos <- vehicles %>% slice(i) %>% select(old.y, old.x) %>% data.matrix()
      new.pos <- vehicles %>% slice(i) %>% select(y, x) %>% data.matrix()
      positions[old.pos] <<- 0
      if (positions[new.pos]) {
        crash <<- TRUE
        return(new.pos)
      } else {
        positions[new.pos] <<- 1
      }
    }
    return(NULL)
  }
  next_epoch <- function() {
    ## go to next epoch
    epochs <<- epochs + 1
    ## and update speeds based on current locations and direction
    vehicles <<- vehicles %>% 
      left_join(tracks, by = c("x", "y")) %>% 
      select(ID, n.turns, x, y, speed.x, speed.y, direction) %>% 
      update_speed()
    
  }
  ## Update positions until a crash is detected
  while (!crash) {
    one_move()
    ## Check that no crash has occured and record position
    crash.position <- check_crash()
    ## If no crash has occured, update epoch and vehicles
    if (!crash) {
      next_epoch()
      cat(glue::glue("End epoch {epochs}"), sep = "\n")
    }
  }
  return(crash.position)
}

play_game_2 <- function(input) {
  x <- format_input(input) 
  ## tracks and vehicles
  tracks   <- build_track(x)
  vehicles <- build_vehicles(x)
  ## Book keeping
  ## 2D matrix for keeping track of collisions
  positions <- matrix(0, 
                      ncol = tracks %>% pull(x) %>% max, 
                      nrow = tracks %>% pull(y) %>% max)
  positions[cbind(vehicles %>% pull(y), vehicles %>% pull(x))] <- (vehicles %>% pull(ID))
  crash <- FALSE
  nb.carts <- nrow(vehicles)
  epochs <- 0
  one_move <- function() {
    ## Order carts based on current location before updating location
    vehicles <<- vehicles %>% select(ID, n.turns, x, y, speed.x, speed.y) %>% 
      arrange(y, x) %>% update_positions()
  }
  check_crash <- function() {
    excluded.ID <- integer(0)
    for (i in 1:nrow(vehicles)) {
      ID <- vehicles %>% slice(i) %>% pull(ID)
      if (!(ID %in% excluded.ID)) {
        old.pos <- vehicles %>% slice(i) %>% select(old.y, old.x) %>% data.matrix()
        new.pos <- vehicles %>% slice(i) %>% select(y, x) %>% data.matrix()
        positions[old.pos] <<- 0
        if (positions[new.pos] != 0) {
          excluded.ID <- c(excluded.ID, ID, positions[new.pos])
          cat(glue::glue("Crash at position {new.pos[2]},{new.pos[1]} between carts {ID} and {positions[new.pos]}"), sep = "\n")
          positions[new.pos] <<- 0
        } else {
          positions[new.pos] <<- ID
        }
      }
    }
    vehicles <<- vehicles %>% filter(! (ID %in% excluded.ID))
    nb.carts <<- nrow(vehicles)
  }
  next_epoch <- function() {
    ## go to next epoch
    epochs <<- epochs + 1
    ## and update speeds based on current locations and direction
    vehicles <<- vehicles %>% 
      left_join(tracks, by = c("x", "y")) %>% 
      select(ID, n.turns, x, y, speed.x, speed.y, direction) %>% 
      update_speed()
  }
  ## Update positions until only one cart is left
  while (nb.carts > 1) {
    one_move()
    ## Check that no crash has occured and record position
    crash.position <- check_crash()
    ## If no crash has occured, update epoch and vehicles
    if (!crash) {
      next_epoch()
      cat(glue::glue("End epoch {epochs}"), sep = "\n")
    }
  }
  return(vehicles)
}

play_game_2_base <- function(input) {
  x <- format_input(input) 
  ## tracks and vehicles
  tracks   <- build_track(x) 
  vehicles <- build_vehicles(x)
}


play_game(test) ## 0.2s, 7,3
play_game(test2) ## 0.2s, 7,3
play_game(input) ## 12s, 32,99

play_game_2(test) ## 0.16s, 6,4
play_game_2(input) ## 0.16s, 6,4
