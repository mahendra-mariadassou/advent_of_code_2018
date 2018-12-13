library(tidyverse)

## store initial state as a binary vector
initial.state <- read_lines("input/day12.txt", n_max = 1) %>% 
  str_remove("initial state: ") %>%
  str_split_fixed(pattern = "", n = Inf) %>% 
  `[`(1, ) %>%
  {if_else(. == "#", 1L, 0L) }

test.state <- c("#..#.#..##......###...###") %>% str_split_fixed("", n = Inf) %>% { if_else(. == "#", 1, 0) }

## store the 2^5 rules in a 2^5 vector
## each pattern is read as integer (+1 to account for 1-based indexing) 
## in binary format and the 
## update rule for that pattern is stored in the corresponding 
## cell of the vector
pattern_to_integer <- function(string) {
  string %>% 
    str_split_fixed("", n = 5) %>% 
    `==`("#") %>% 
    `*`(matrix(rep(2^(0:4), length(string)), ncol = 5, byrow = T)) %>% 
    rowSums(na.rm = T) %>% 
    `+`(1)
}

## Numeric version of pattern_to_integer for binarized patterns
## vectorized over arguments
vectors_to_integer <- function(x1, x2, x3, x4, x5) {
  cbind(x1, x2, x3, x4, x5) %>% 
    `*`(matrix(rep(2^(0:4), nrow(.)), ncol = 5, byrow = T)) %>% 
    rowSums(na.rm = T) %>% 
    `+`(1)
}

create_rules <- function(input) {
  d <- input %>% 
    tibble(raw = .) %>% 
    separate(col = raw, into = c("pattern", "result"), sep = " => ") %>% 
    mutate(result  = if_else(result == "#", 1, 0), 
           pos     = pattern_to_integer(pattern))
  template <- rep(0, 32)
  template[d$pos] <- d$result
  template
}

input <- read_lines("input/day12.txt", skip = 2)
test.input <- c("...## => #",
                "..#.. => #",
                ".#... => #",
                ".#.#. => #",
                ".#.## => #",
                ".##.. => #",
                ".#### => #",
                "#.#.# => #",
                "#.### => #",
                "##.#. => #",
                "##.## => #",
                "###.. => #",
                "###.# => #",
                "####. => #")

test.rules <- create_rules(test.input)
rules <- create_rules(input)

pass_one_generation <- function(state, rules) {
  patterns.as.integer <- vectors_to_integer(x1 = lag(state, 2), 
                                            x2 = lag(state, 1), 
                                            x3 = state, 
                                            x4 = lead(state, 1), 
                                            x5 = lead(state, 2) 
                                            )
  rules[patterns.as.integer]
}

expand_borders <- function(df) {
  bind_rows(tibble(state = 0, pos = df %>% slice(1) %>% pull(pos) %>% `-`(1)), 
            df, 
            tibble(state = 0, pos = df %>% slice(n()) %>% pull(pos) %>% `+`(1)))
}

trim_first <- function(df) {
  first.1.position <- df %>% pull(state) %>% which.max %>% `-`(1)
  if (first.1.position) { return(df %>% slice(-(1:first.1.position))) }
  df
}

trim_last <- function(df) {
  if (df %>% pull(state) %>% last %>% `==`(0)) return(df %>% slice(-n()))
  df
}

print_state <- function(x) { x %>% as.logical %>% if_else("#", ".") %>% str_c(collapse = "")}

pass_time <- function(initial.state, n.gen, rules) {
  ## spreads at most one position on the left and one position on right at each generation
  cur.state <- tibble(state = initial.state) %>% 
    mutate(pos = row_number() - 1)
  while (n.gen > 0) {
    cur.state <- cur.state %>% 
      expand_borders() %>% 
      mutate(state = pass_one_generation(state, rules)) %>% 
      trim_first %>% trim_last
    n.gen <- n.gen -1
  }
  cur.state
}

## search for a limit cycle and record the generation at which the limit cycle is reached
find_limit <- function(initial.state, rules) {
  ## Book keeping 
  tmp <- tibble(state = initial.state) %>% 
    mutate(pos = row_number() - 1)
  converged <- FALSE
  old.state <- initial.state
  nb.iter <- 0
  ## Do 1 generation update
  pass_one_generation_fast <- function(state) {
    patterns.as.integer <- vectors_to_integer(x1 = lag(state, 2), 
                                              x2 = lag(state, 1), 
                                              x3 = state, 
                                              x4 = lead(state, 1), 
                                              x5 = lead(state, 2) 
    )
    rules[patterns.as.integer]
  } 
  while (!converged) {
    tmp <- tmp %>% expand_borders() %>% 
      mutate(state = pass_one_generation_fast(state)) %>% 
      trim_first %>% trim_last
    converged <- (length(old.state) == length(pull(tmp, state))) && all(old.state == pull(tmp, state))
    old.state <- pull(tmp, state)
    nb.iter <- nb.iter + 1
  }
  return(tmp %>% expand_borders() %>% mutate(new = pass_one_generation_fast(state), generation = nb.iter+1)) 
}

solve_problem_1 <- function(initial.state, n.gen, rules) {
  pass_time(initial.state, n.gen, rules) %>% 
    filter(state == 1) %>% 
    pull(pos) %>% sum()
}

solve_problem_2 <- function(initial.state, n.gen, rules) {
  x <- find_limit(initial.state, rules)
  step.size <- which.max(x %>% pull(new)) - which.max(x %>% pull(state))
  x %>% 
    filter(new == 1) %>% 
    transmute(new.pos = pos + step.size * (n.gen - generation)) %>% 
    pull() %>% 
    sum() %>%
    as.character()
}

## solve_problem_1(test.state, 20, test.rules) ## 0.02s, 325
solve_problem_1(initial.state, 20, rules) ## 0.02s, 1816

solve_problem_2(initial.state, 5e10, rules) 
