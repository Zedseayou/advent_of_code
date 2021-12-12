library(tidyverse)

parse_input <- function(input) {
  edges <- read_delim(input, delim = "-", col_names = c("from", "to"))

  reversed <- edges %>%
    filter(from != "start", to != "end") %>%
    select(from = to, to = from)

  bind_rows(edges, reversed)
}

test_12a <- parse_input("start-A
start-b
A-c
A-b
b-d
A-end
b-end")

test_12a

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

# until reached the end:
# start with a node
# choose a next node to go to, from options in branches\
# if it's small, keep track
# once at the end:
# compare to existing paths found already
# if path already found, retreat back to last node with other options
# do this by going back one node, and checking if there are other options
# compare set of options at that node to matching paths()
# if there is a available node not already in a matching path, take it
# if not, retreat one more node

# For edge list, current node, and existing path: show allowed next nodes
node_options <- function(input, node, path) {
  small <- str_subset(path, "^[a-z]+$")
  input %>%
    filter(from == node, !(to %in% small)) %>%
    pull(to)
}

node_options(test_12a, "start", "start")
node_options(test_12a, "d", c("d", "b", "start"))
node_options(test_12a, "A", c("A", "b", "start"))

# Find a valid forward path from a point
find_path <- function(input, node = "start", path = "start") {
  current <- node
  while (current != "end") {
    options <- node_options(input, current, path)
    if (length(options) == 0) {
      return(NULL) # found a dead end
    }
    current <- sample(options, 1)
    path <- c(current, path) #  paths stored in reverse
  }
  path
}

find_path(test_12a)
find_path(test_12a, "d", c("d", "b", "start"))
find_path(test_12a, "A", c("A", "b", "start"))

# For a given path and set of paths, backtrack to a node where the two could diverge
backtrack_path <- function(input, path) {
  new_path <- path
  options <- character()
  while (length(options) == 0) {
    prev_to <- new_path[1]
    new_path <- new_path[-1]
    options <- node_options(input, node = new_path[1], path = new_path)
    options <- options[options != prev_to]
  }
  c(sample(options, 1), new_path)
}

backtrack_path(test_12a, path = c("end", "b", "start"))

find_new_path <- function(input, path, paths) {
  new_path <- find_path(input)
  while (has_element(paths, new_path) | is.null(new_path)) {
    backtracked <- backtrack_path(input, new_path)
    new_path <- find_path(input, node = new_path[1], path = new_path)
    Sys.sleep(0.5)
    print(new_path)
  }
  new_path
}

find_new_path(test_12a, path = c("end", "b", "start"), paths = list(c("end", "b", "start")))

q12a <- function(input) {
  paths <- list()

}

q12a(test_12a)
