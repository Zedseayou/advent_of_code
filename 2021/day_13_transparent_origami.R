library(tidyverse)

parse_input <- function(input) {
  lines <- read_lines(input)
  folds <- lines[which(lines == ""):length(lines)]
  coords <- setdiff(lines, folds) %>%
    enframe(name = NULL) %>%
    separate(value, c("x", "y"), ",") %>%
    mutate(across(.fns = ~ as.integer(.x)))
  folds <- folds[-1] %>%
    str_remove_all("fold along ") %>%
    enframe(name = NULL) %>%
    separate(value, c("axis", "value"), "=") %>%
    mutate(value = as.integer(value))

  list(coords = coords, folds = folds)
}

test_13 <- parse_input("6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

input_13 <- parse_input("2021/data/input_13.txt")

view_grid <- function(input_coords) {
  coords <- mutate(input_coords, across(.fns = ~ .x + 1L))
  ncols <- max(coords$x)
  nrows <- max(coords$y)

  grid <- matrix(rep(".", length.out = ncols * nrows), ncol = ncols)
  coords
  grid[as.matrix(coords[, c(2, 1)])] <- "#"
  # grid[14, 0] < "#"
  grid
}

view_grid(test_13$coords)

perform_fold <- function(input_coords, axis = c("x", "y"), value) {
  coords <- input_coords
  p <- coords[[axis]]
  value <- as.integer(value)

  coords[[axis]] <- if_else(p > value, (2L * value) - p, p)
  # coords[[axis]] <- coords[[axis]] + (0 - min(coords[[axis]]))
  unique(coords, MARGIN = 1)
}

perform_fold(test_13$coords, "y", 7) %>% view_grid()
test_13$coords %>%
  perform_fold("y", 7) %>%
  perform_fold("x", 5) %>%
  view_grid()

q13a <- function(input) {
  input$coords %>%
    perform_fold(input$folds[["axis"]][1], input$folds[["value"]][1]) %>%
    nrow()
}

q13a(test_13)
q13a(input_13)


q14a <- function(input) {
  reduce2(
    .x = input$folds$axis,
    .y = input$folds$value,
    .f = perform_fold,
    .init = input$coords
  )
}

q14a(test_13) %>% view_grid()
q14a(input_13) %>% view_grid() # BFKRCJZU
