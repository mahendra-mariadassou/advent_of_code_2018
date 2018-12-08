library(tidyverse)

test <- c("Step C must be finished before step A can begin.",
          "Step C must be finished before step F can begin.",
          "Step A must be finished before step B can begin.",
          "Step A must be finished before step D can begin.",
          "Step B must be finished before step E can begin.",
          "Step D must be finished before step E can begin.",
          "Step F must be finished before step E can begin.")

input <- read_lines("input/day7.txt")

format_input <- function(x) {
  x %>% 
    tibble(raw = .) %>% 
    separate(raw, into = str_c("V", 1:10), sep = " ") %>%
    select(V2, V8) %>%
    rename(parent = V2, child = V8)
}

sort_nodes <- function(graph) {
  node.set <- graph %>% {c(.$child, .$parent)} %>% unique() %>% sort()
  node.order <- set_names(rep(0, length(node.set)), node.set)
  ## Steps that can be activated
  active_nodes <- function() {
    anti_join(tibble(node = node.set), graph, by = c("node" = "child")) %>% pull(node)
  }
  ## Remove a step and all corresponding constraints
  remove_node <- function(node) {
    node.set <<- node.set[node.set != node]
    graph <<- filter(graph, parent != node)
  }
  ## Sort nodes
  step.number <- 1
  while (length(node.set)) {
    node <- active_nodes()[1]
    node.order[node] <- step.number
    step.number <- step.number + 1
    remove_node(node)
  }  
  ## return result
  sort(node.order) %>% names() %>% str_flatten()
}

monitor_tasks <- function(graph, lag = 60, nb.elves = 6) {
  ## Initialize all tables / variables required for bookkeeping
  schedule <- tibble(node        = graph %>% {c(.$child, .$parent)} %>% unique(), 
                     duration    = lag + match(node, LETTERS), 
                     complete.in = Inf,
                     elve        = NA_integer_) %>% 
    arrange(node)
  idle.elves <- rep(T, nb.elves) ## which elves are busy
  elapsed.time <- 0
  ## Find a task (i) not already assigned to an elve and with (ii) no unsatisfied constraints or return NA
  active_nodes <- function() {
    anti_join(schedule, graph, by = c("node" = "child")) %>% 
      filter(is.na(elve)) %>% 
      pull(node) %>% 
      `[`(1)
  }
  ## Remove a step and all corresponding constraints (free elve and remove step from graph)
  remove_node <- function(.node) {
    elves <- schedule %>% filter(node == .node) %>% pull(elve)
    idle.elves[elves] <<- T
    schedule <<- filter(schedule, node != .node)
    graph <<- filter(graph, parent != .node)
  }
  ## Update remaining duration time of all steps
  ## Update current time
  update_duration <- function(time) {
    schedule <<- mutate(schedule, complete.in = complete.in - time)
    elapsed.time <<- elapsed.time + time
  }
  ## Assign an elve to a node
  activate_node <- function(.node, .elve) {
    schedule <<- schedule %>% mutate(complete.in = if_else(node == .node, duration, complete.in), 
                                     elve        = if_else(node == .node, .elve, elve))
    idle.elves[.elve] <<- F
  }
  ## Find all finished tasks
  finished_tasks <- function() { schedule %>% filter(complete.in == 0) %>% pull(node) }
  ## Time until next task is completed
  waiting_time <- function() { schedule %>% pull(complete.in) %>% min }
  ## Find an idle elve (or return NA)
  idle_elve <- function() { 
    if (all(!idle.elves)) {
      return(NA_integer_)
    } 
    min(which(idle.elves))
  }
  ## 
  while (nrow(schedule)) {
    elve <- idle_elve()
    node <- active_nodes()[1]
    if (!is.na(elve) & !is.na(node)) {
      activate_node(node, elve)
    }
    else {
      update_duration(waiting_time())
      completed.nodes <- finished_tasks()
      for (node in completed.nodes) {
        remove_node(node)
      }
    }
  }
  ## return result
  elapsed.time
}

solve_problem_1 <- function(input) { input %>% format_input() %>% sort_nodes() }

# solve_problem_1(test) # 0.016, CABDFE
solve_problem_1(input) # 0.0386, EPWCFXKISTZVJHDGNABLQYMORU

solve_problem_2 <- function(input, lag, nb.elves) { input %>% format_input() %>% monitor_tasks(lag, nb.elves) }

# solve_problem_2(test, 0, 2) # 0.036, 15
solve_problem_2(input, lag = 60, nb.elves = 6) # 0.113, 952
