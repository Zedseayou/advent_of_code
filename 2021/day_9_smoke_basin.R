library(tidyverse)

parse_input <- function(raw) {
  width = read_lines(raw, n_max = 1) %>% str_length
  read_fwf(raw, fwf_widths(rep(1, width))) %>%
    as.matrix()
}

test_9 <- parse_input("2199943210
3987894921
9856789892
8767896789
9899965678")

input_9 <- parse_input("2021/data/input_9.txt")

find_minima <- function(line) {
  line < lag(line, default = Inf) & line < lead(line, default = Inf)
}

find_minima(test_9[1,])

q9a <- function(input) {
  rows <- apply(input, 1, find_minima) %>% t
  cols <- apply(input, 2, find_minima)

  sum(input[rows & cols] + 1)
}

q9a(test_9)
q9a(input_9)

apply_line <- function(input, fun, combine) {
  rows <- apply(input, 1, fun) %>% t
  cols <- apply(input, 2, fun)
  if (combine == "and") {
    return(rows & cols)
  } else if (combine == "or") {
    return(rows | cols)
  }
}
apply_line(test_9, find_minima, "and")

find_basin_potential <- function(line) {
  ((line > lead(line)) | (line > lag(line))) & line != 9
}

apply_line(test_9, find_basin_potential, "or")

next_to_basin <- function(basin_line) {
  lead(basin_line, default = FALSE) | lag(basin_line, default = FALSE)
}

apply_line(apply_line(test_9, find_minima, "and"), next_to_basin, "or")

expand_basin <- function(input, basin) {
  potential <- apply_line(input, find_basin_potential, "or")
  adjacent <- apply_line(basin, next_to_basin, "or")

  new_basin <- (potential & adjacent) | basin
  # new_basin[is.na(new_basin)] <- FALSE
  new_basin
}

expand_basin(test_9, apply_line(test_9, find_minima, "and"))

find_basin <- function(input) {
  minima <- apply_line(input, find_minima, "and")

  old_basin <- FALSE
  basin <- minima
  while (!all(basin == old_basin)) {
  # for (i in 1:5) {
    old_basin <- basin
    basin <- expand_basin(input, basin)
  }
  basin
}
find_basin(test_9)

basin_size <- function(row, col, basin) {
  basin_members <- list(as.numeric(c(row, col)))
  checked <- FALSE

  while (any(checked == FALSE)) {
    to_check_i <- which(checked == FALSE)[1]
    # print(c(i = to_check_i))
    checked[to_check_i] <- TRUE
    row <- basin_members[[to_check_i]][1]
    col <- basin_members[[to_check_i]][2]

    neighbours <- list(
      c(row + 1, col),
      c(row - 1, col),
      c(row, col + 1),
      c(row, col - 1)
    ) %>%
      discard(~ any(.x < 1 | .x[1] > nrow(basin) | .x[2] > ncol(basin)))
    for (n in neighbours) {
      if (isTRUE(basin[n[1], n[2]])) {
        if(!has_element(basin_members, n)) {
          basin_members <- append(basin_members, list(n))
          checked <- c(checked, FALSE)
        }
      }
    }
  }
  length(basin_members)
}
test_basin <- find_basin(test_9)
basin_size(1L, 2L, test_basin)

q6b <- function(input) {
  basin <- find_basin(input)
  minima <- apply_line(input, find_minima, "and") %>%
    which(arr.ind = TRUE) %>%
    as_tibble()

  minima %>%
    mutate(size = map2_int(row, col, basin_size, basin = basin)) %>%
    pull(size) %>%
    sort(decreasing = TRUE) %>%
    head(3) %>%
    prod()
}

q6b(test_9)
q6b(input_9)
