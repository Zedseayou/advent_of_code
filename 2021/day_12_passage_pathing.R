library(tidyverse)

parse_input <- function(input) {
  edges <- read_delim(input, delim = "-", col_names = c("from", "to"))

  reversed <- select(edges, from = to, to = from)

  bind_rows(edges, reversed)
}

test_12a <- parse_input("start-A
start-b
A-c
A-b
b-d
A-end
b-end")

test_12b <- parse_input("dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

test_12c <- parse_input("fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW")

input_12 <- parse_input("2021/data/input_12.txt")

# For edge list, current node, and existing path: show allowed next nodes
node_options <- function(input, node, path) {
  small <- str_subset(path, "^[a-z]+$")
  if (node == "end") {
    out <- character()
  } else {
    out <- input$to[input$from == node & !(input$to %in% small)]
  }
  out
}

node_options(test_12a, "start", "start")
node_options(test_12a, "d", c("d", "b", "start"))
node_options(test_12a, "A", c("A", "b", "start"))

extend_path <- function(path, input, part = c("a", "b")) {
  node_fun <- switch(
    part,
    a = node_options,
    b = node_options_2
  )

  opts <- node_fun(input, node = path[1], path)
  if (length(opts) == 0) { # terminal paths left untouched
    out <- list(path)
  } else {
    out <- map2(.x = rep(list(path), length(opts)), .y = opts, .f = ~c(.y, .x))
  }
  out
}

extend_path(c("A", "start"), test_12a, part = "a")
extend_path(c("end", "A", "start"), test_12a, part = "a")

find_paths <- function(input, part = c("a", "b")) {
  old_paths <- list("start")
  repeat {
    new_paths <- old_paths %>%
      map(extend_path, input = input, part = part) %>%
      flatten()

    if (identical(new_paths, old_paths)) {
      break
    }

    old_paths <- new_paths
  }

  new_paths %>%
    keep(.p = ~.x[1] == "end")
}

q12a <- function(input) {
  input %>%
    find_paths(part = "a") %>%
    length()
}

q12a(test_12a) # 10
q12a(test_12b) # 19
q12a(test_12c) # 226
q12a(input_12) # 3887

node_options_2 <- function(input, node, path) {
  small <- str_subset(path, "^[a-z]+$")
  # Caves allowed if not a visited small cave, or if no cave yet visited twice
  allowed_small <- !(input$to %in% small) | all(table(small) <= 1)

  if (node == "end") {
    out <- character()
  } else {
    from_node <- input$from == node
    out <- input$to[input$from == node & allowed_small]
  }
  out[out != "start"] # start never allowed
}

node_options_2(test_12a, "start", "start")
node_options_2(test_12a, "d", c("d", "b", "start"))
node_options_2(test_12a, "A", c("A", "b", "start"))

q12b <- function(input) {
  input %>%
    find_paths(part = "b") %>%
    length()
}

q12b(test_12a) # 36
q12b(test_12b) # 103
q12b(test_12c) # 3509
q12b(input_12) # 104834
