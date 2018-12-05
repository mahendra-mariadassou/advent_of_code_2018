library(tidyverse)
library(lubridate)
## input

test <- c(
"[1518-11-01 00:00] Guard #10 begins shift",
"[1518-11-01 00:05] falls asleep",
"[1518-11-01 00:25] wakes up",
"[1518-11-01 00:30] falls asleep",
"[1518-11-01 00:55] wakes up",
"[1518-11-01 23:58] Guard #99 begins shift",
"[1518-11-02 00:40] falls asleep",
"[1518-11-02 00:50] wakes up",
"[1518-11-03 00:05] Guard #10 begins shift",
"[1518-11-03 00:24] falls asleep",
"[1518-11-03 00:29] wakes up",
"[1518-11-04 00:02] Guard #99 begins shift",
"[1518-11-04 00:36] falls asleep",
"[1518-11-04 00:46] wakes up",
"[1518-11-05 00:03] Guard #99 begins shift",
"[1518-11-05 00:45] falls asleep",
"[1518-11-05 00:55] wakes up"
)

input <- read_lines("input/day4.txt")

format_input <- function(x) {
  x %>% 
    tibble(raw = .) %>% 
    mutate(time      = str_replace_all(raw, "\\[|\\].*", "") %>% ymd_hm(), 
           guard     = str_extract(raw, "Guard #[0-9]*") %>% str_remove("Guard #") %>% as.integer(), 
           sleeps    = str_detect(raw, "falls asleep"), 
           wakes     = str_detect(raw, "wakes"), 
           minute    = minute(time)) %>% 
    arrange(time) %>% 
    mutate(block = cumsum(!is.na(guard))) %>% 
    group_by(block) %>% 
    mutate(guard = guard[1], event = (1:n()) %/% 2, type = if_else(sleeps, "start", "end")) %>%
    filter(event > 0) %>% 
    select(guard, minute, event, type, block) %>% 
    spread(key = type, value = minute)
}

solve_problem_1 <- function(input) {
  format_input(input) %>% 
    group_by(guard) %>% 
    nest() %>% 
    mutate(
      ## total sleep duration 
      sleep.duration = map_int(data, ~ sum(.$end - .$start)), 
      ## create hour long vector and record how many times the guard was asleep at any given minute
      distribution = map(data, function(data) {
        time <- rep(0, 60)
        pwalk(data %>% select(start, end), function(start, end) { time[(start+1):end] <<- time[(start+1):end]+1 })
        time
      }), 
      ## most slept minute
      most.sleepy = map_int(distribution, which.max) - 1, 
      result = most.sleepy * guard
    )  %>% 
    arrange(desc(sleep.duration)) %>% 
    slice(1) %>% 
    pull(result)  
}

solve_problem_2 <- function(input) {
  format_input(input) %>% 
    group_by(guard) %>% 
    nest() %>% 
    mutate(
      ## total sleep duration 
      sleep.duration = map_int(data, ~ sum(.$end - .$start)), 
      ## create hour long vector and record how many times the guard was asleep at any given minute
      distribution = map(data, function(data) {
        time <- rep(0L, 60)
        pwalk(data %>% select(start, end), function(start, end) { time[(start+1):end] <<- time[(start+1):end]+1L })
        time
      }), 
      ## most slept minute
      most.sleepy = map_int(distribution, which.max) - 1, 
      ## number of times it is slept
      nb.sleeps = map_int(distribution, max), 
      result = most.sleepy * guard
    )  %>% 
    arrange(desc(nb.sleeps)) %>% 
    slice(1) %>% 
    pull(result)  
}

## solve_problem_1(test)
solve_problem_1(input) ## 0.34 s, 39698

## solve_problem_2(test)
solve_problem_2(input) ## 0.29s, 14920
