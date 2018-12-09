library(tidyverse) 

test <- c("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2") %>% str_split(pattern = " ", n = Inf) %>% unlist() %>% as.integer()
input <- read_lines("input/day8.txt") %>% str_split(pattern = " ", n = Inf) %>% unlist() %>% as.integer()


parse_tree <- function(x) {
  ## book keeping
  node.list <- list()
  pos <- 3
  cur.node <- 1
  new.node <- 2
  ## Helper functions to build node
  build_node <- function(node, parent, n.child, n.metadata) {
    node.list[[node]] <<- list(
      node       = node,
      parent     = parent,
      n.child    = n.child,
      n.vis.ch   = 0,     ## Number of visited children
      n.metadata = n.metadata,
      complete   = (n.child == 0), ## Have all children been visited yet?
      children   = rep(NA, n.child),
      metadata   = rep(NA, n.metadata)
    )    
  }
  ## add a children to a parent node and verify whether all children have been visited yet
  add_child <- function(parent, child) {
    node <- node.list[[parent]]
    node$n.vis.ch <- node$n.vis.ch + 1
    node$children[node$n.vis.ch] <- child
    if (node$n.vis.ch == node$n.child) {
      node$complete <- TRUE
    }
    node.list[[parent]] <<- node
  }
  ## If reading a new node, create and store it in node.list
  read_node <- function() {
    ## get ID of new node and update child, parent and new.node
    parent <- cur.node
    cur.node <<- new.node
    new.node <<- new.node + 1
    ## build new node
    build_node(node = cur.node, parent = parent, n.child = x[pos], n.metadata = x[pos + 1])
    ## Add child to its parent node
    add_child(parent = parent, child = cur.node)
    ## update pos
    pos <<- pos + 2
  }
  ## If reading metadata, store them in current node and update current node to its parent
  read_metadata <- function() {
    n.metadata <- node.list[[cur.node]]$n.metadata
    if (n.metadata > 0) {
      node.list[[cur.node]]$metadata <<- x[(pos - 1) + 1:n.metadata]
    }
    ## Update current node
    cur.node <<- node.list[[cur.node]]$parent
    ## Update pos 
    pos <<- pos + n.metadata
  }
  ## Helper functions to detect status of node
  is_metadata <- function() {
    ## we only start reading metadata for a node after having visited all its childrent
    node.list[[cur.node]]$complete
  }
  is_node <- function() { !is_metadata() }
  ## Build root
  build_node(node = 1, parent = 0, x[1], x[2])
  ## Parse the rest of the input
  while (pos < length(x)) {
    if (is_node()) {
      read_node()
    } else {
      read_metadata()
    }
  }
  node.list
}

solve_problem_1 <- function(input) {
  parse_tree(input) %>% map_int(~ sum(.x$metadata)) %>% sum()
}

## solve_problem_1(test) ## 0.007s, 138
solve_problem_1(input) ## 0.028s, 46962

solve_problem_2 <- function(input) {
  tree <- input %>% parse_tree
  values <- rep(NA, length(tree))
  get_value <- function(nodes) {
    values[nodes]
  }
  set_value <- function(node) {
    if (node$n.child == 0) {
      value <- sum(node$metadata)
    } else {
      children <- node$children[node$metadata] %>% {.[!is.na(.)] }
      value <- children %>% get_value %>% sum
    }
    values[node$node] <<- value
  }
  ## Tree was constructed using a depth-first search, 
  ## Values can be computed by traversing the tree from the leaves to the root, 
  ## i.e. in the reverse order from the one they are in in list
  walk(rev(tree), set_value)
  return(values[1])
}

## solve_problem_2(test) ## 0.017s, 66
solve_problem_2(input) ## 0.152, 22633
