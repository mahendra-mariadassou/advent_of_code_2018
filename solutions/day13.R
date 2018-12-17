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
                  "<"  = "horizontal", 
                  ">"  = "horizontal", 
                  "+"  = "intersection", 
                  "/"  = "top.left.corner", 
                  "\\" = "top.right.corner",
                  " "  = "")
  raw <- input %>% str_split_fixed(pattern = "", n = Inf)
  matrix(dictionary[raw], nrow = nrow(raw))
}

## build_track_base(test2)

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
           direction = resolve_intersection(direction, speed.x, speed.y, n.turns), 
           ## use simple composition rules to update speeds: speed (1, 0) + top.left.corner "/" = speed (0, -1)
           ## top.right.border exchanges speed.x and speed.y
           ## top.left.border inverts speed.x and speed.y before exchanging them
           corner      = str_detect(direction, "corner"), 
           new.speed.x = if_else(corner, speed.y, speed.x) * if_else(direction == "top.left.corner", -1, 1), 
           new.speed.y = if_else(corner, speed.x, speed.y) * if_else(direction == "top.left.corner", -1, 1), 
           speed.x = new.speed.x, 
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

print_state <- function(tracks, vehicles, vec.only = TRUE) {
  plotdata <- left_join(tracks, vehicles, by = c("x", "y")) %>%
    mutate(vehicle = !is.na(ID), 
           segment = case_when(speed.x ==  1 ~ ">",
                               speed.x == -1 ~ "<", 
                               speed.y ==  1 ~ "v",
                               speed.y == -1 ~ "^",
                               TRUE          ~ segment))
  p <- ggplot(plotdata, aes(x = x, y = -y, label = `segment`)) + 
    geom_tile() +
    geom_text(data = if (vec.only) { filter(plotdata, vehicle) } else { plotdata }, 
              color = "red", size = 10)
  plot(p)
}

play_game <- function(input) {
  x <- format_input(input) 
  ## tracks and vehicles
  tracks   <- build_track_base(input)
  vehicles <- build_vehicles(x) %>% mutate(old.x = x, old.y = y, direction = "straight")
  y.max <- nrow(tracks); x.max <- ncol(tracks)
  ## Book keeping
  ## 2D matrix for keeping track of collisions
  positions <- matrix(0, 
                      ncol = x.max, 
                      nrow = y.max)
  positions[cbind(vehicles %>% pull(y), vehicles %>% pull(x))] <- 1
  crash <- FALSE
  epochs <- 0
  one_move <- function() {
    ## Order carts based on current location before updating location
    vehicles[, ] <<- vehicles %>% arrange(y, x) %>% update_positions()
  }
  check_crash <- function() {
    for (i in 1:nrow(vehicles)) {
      ## old.pos <- vehicles[i, c("old.y", "old.x")] %>% data.matrix()
      old.pos <- `dim<-`(c(vehicles[[8]][i], vehicles[[7]][i]), c(1, 2))
      ## new.pos <- vehicles[i, c("y", "x")] %>% data.matrix()
      new.pos <- `dim<-`(c(vehicles[[1]][i], vehicles[[2]][i]), c(1, 2))
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
    vehicles[,] <<- vehicles %>% 
      ## update directions 
      mutate(direction   = tracks[(x-1)*y.max + y], 
             ## set new speeds accordingly
             n.turns     = if_else(direction == "intersection", n.turns + 1, n.turns), 
             direction   = resolve_intersection(direction, speed.x, speed.y, n.turns), 
             corner      = str_detect(direction, "corner"), 
             tmp.speed.x = if_else(corner, speed.y, speed.x) * if_else(direction == "top.left.corner", -1, 1), 
             speed.y = if_else(corner, speed.x, speed.y) * if_else(direction == "top.left.corner", -1, 1), 
             speed.x = tmp.speed.x) %>% 
      select(-tmp.speed.x, -corner)
  }
  ## Update positions until a crash is detected
  while (!crash) {
    one_move()
    ## Check that no crash has occured and record position
    crash.position <- check_crash()
    ## If no crash has occured, update epoch and vehicles
    if (!crash) {
      next_epoch()
      ## cat(glue::glue("End epoch {epochs}"), sep = "\n")
    }
  }
  return(crash.position)
}

play_game_2 <- function(input, max.epochs = 1000) {
  x <- format_input(input) 
  ## tracks and vehicles
  tracks   <- build_track_base(input)
  vehicles <- build_vehicles(x) %>% mutate(old.x = x, old.y = y, direction = "straight")
  not.crashed  <- rep(TRUE, nrow(vehicles)) ## crashed.vehicles
  ## Book keeping
  ## 2D matrix for keeping track of collisions
  y.max <- nrow(tracks); x.max <- ncol(tracks)
  ## Book keeping
  ## 2D matrix for keeping track of collisions
  positions <- matrix(0, 
                      ncol = x.max, 
                      nrow = y.max)
  positions[cbind(vehicles %>% pull(y), vehicles %>% pull(x))] <- (vehicles %>% pull(ID))
  crash <- FALSE
  nb.carts <- length(not.crashed)
  epochs <- 0
  one_move <- function() {
    ## Order carts based on current location before updating location
    vehicles[, ] <<- vehicles %>% arrange(y, x) %>% update_positions()
  }
  check_crash <- function() {
    update.vehicle <- FALSE
    for (i in 1:nrow(vehicles)) {
      ID <- vehicles[[3]][i] ## Column 3 = ID
      if (not.crashed[ID]) {
        ## Fast version of 
        ## old.pos <- vehicles %>% slice(i) %>% select(old.y, old.x) %>% data.matrix()
        ## column 8 = y, column 7 = x
        old.pos <- `dim<-`(c(vehicles[[8]][i], vehicles[[7]][i]), c(1, 2))
        ## new.pos <- vehicles[i, c("y", "x")] %>% data.matrix()
        ## column 1 = y, column 2 = x
        new.pos <- `dim<-`(c(vehicles[[1]][i], vehicles[[2]][i]), c(1, 2))
        positions[old.pos] <<- 0
        if (positions[new.pos] != 0) {
          not.crashed[c(ID, positions[new.pos])] <<- FALSE
          cat(glue::glue("Crash at position {new.pos[2]},{new.pos[1]} between carts {ID} and {positions[new.pos]} during tick {epochs}"), sep = "\n")
          positions[new.pos] <<- 0
          nb.carts <<- nb.carts - 2
          update.vehicle <- TRUE
        } else {
          positions[new.pos] <<- ID
        }
      }
    }
    if (update.vehicle) vehicles <<- vehicles %>% filter(ID %in% which(not.crashed))
  }
  next_epoch <- function() {
    ## go to next epoch
    epochs <<- epochs + 1
    vehicles[,] <<- vehicles %>% 
      ## update directions 
      mutate(direction   = tracks[(x-1)*y.max + y], 
             ## set new speeds accordingly
             n.turns     = if_else(direction == "intersection", n.turns + 1, n.turns), 
             direction   = resolve_intersection(direction, speed.x, speed.y, n.turns), 
             corner      = str_detect(direction, "corner"), 
             tmp.speed.x = if_else(corner, speed.y, speed.x) * if_else(direction == "top.left.corner", -1, 1), 
             speed.y = if_else(corner, speed.x, speed.y) * if_else(direction == "top.left.corner", -1, 1), 
             speed.x = tmp.speed.x) %>% 
      select(-tmp.speed.x, -corner)
  }
  ## Update positions until only one cart is left
  while (nb.carts > 1 & epochs < max.epochs) {
    one_move()
    ## Check that no crash has occured and record position
    crash.position <- check_crash()
    ## If no crash has occured, update epoch and vehicles
    if (!crash) {
      next_epoch()
      if (epochs %% 500 == 0) cat(glue::glue("End tick {epochs}"), sep = "\n")
    }
  }
  return(vehicles)
}

play_game(test) ## 0.2s, 7,3
play_game(test2) ## 0.2s, 7,3
play_game(input) ## 1.5s, 32,99


## play_game_2(test2) ## 0.16s, 6,4
play_game_2(input, max.epochs = Inf) ## 110s; 56,31