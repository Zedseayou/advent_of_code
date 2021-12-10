library(tidyverse)

input_4_numbers <- read_lines("2021/data/input_04.txt", n_max = 1) %>%
  str_split(",") %>%
  `[[`(1) %>%
  as.integer()
input_4_grids <- read_lines("2021/data/input_04.txt", skip = 2)

test_4_numbers <- str_split("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1", ",") %>%
  `[[`(1) %>%
  as.integer()

test_4_grids <- read_lines("22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

parse_grids <- function(input) {
  grid_lines <- str_subset(input, "^$", negate = TRUE)

  grids <- map2(
    seq(1, length(grid_lines) - 4, by = 5),
    seq(5, length(grid_lines), by = 5),
    ~ grid_lines[seq(.x, .y)]
  ) %>%
    map(
      .f = ~ .x %>%
        str_c(collapse = " ") %>%
        str_squish() %>%
        str_split(" ") %>%
        `[[`(1) %>%
        as.integer() %>%
        matrix(ncol = 5, byrow = TRUE)
    )
  grids
}
grids <- parse_grids(input_4_grids)
test_grids <- parse_grids(test_4_grids)

replace_number <- function(grid, num) {
  grid[grid == num] <- 0
  grid
}

check_win <- function(grid) {
  won_row <- map_lgl(grid, ~ any(rowSums(.x) == 0))
  won_col <- map_lgl(grid, ~ any(colSums(.x) == 0))
  which(won_row | won_col)
}

parse_grids(c("0 0 0 0 0", rep("1 1 1 1 1", 4))) %>% check_win()

q4a <- function(input, grids) {
  call_number <- function(grids, num) {
    grids <- map(grids, replace_number, num = num) # Mark all grids with num
    won <- check_win(grids)
    if (length(won) > 0) { # At least one grid has won
      out <- done(list(num, grids[[won]]))
    } else {
      out <- grids
    }
    out
  }
  winning_grid <- reduce(input, call_number, .init = grids)
  winning_grid[[1]] * sum(winning_grid[[2]])
}
q4a(test_4_numbers, test_grids)
q4a(input_4_numbers, grids)

q4b <- function(input, grids) {
  call_number <- function(grids, num) {
    new_grids <- map(grids, replace_number, num = num)
    won <- check_win(new_grids)
    if (length(won) == length(grids)) {
      last_grid_num <- setdiff(won, check_win(grids))[1]
      out <- done(list(num, new_grids[[last_grid_num]]))
    } else {
      out <- new_grids
    }
    out
  }
  last_grid <- reduce(input, call_number, .init = grids)
  last_grid[[1]] * sum(last_grid[[2]])
}
q4b(test_4_numbers, test_grids)
q4b(input_4_numbers, grids)
