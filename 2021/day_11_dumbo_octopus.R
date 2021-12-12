library(tidyverse)

parse_input <- function(raw) {
  raw %>%
    read_lines() %>%
    str_split("") %>%
    enframe(name = NULL, value = "energy") %>%
    rowid_to_column(var = "row") %>%
    mutate("col" = map(energy, ~ 1:length(.x))) %>%
    unnest(c(col, energy)) %>%
    select(row, col, energy) %>%
    mutate(energy = as.integer(energy))
}

test_11 <-"5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526" %>% parse_input()

test_11b <- "11111
19991
19191
19991
11111" %>% parse_input()

input_11 <- parse_input("2021/data/input_11.txt")

view_grid <- function(input) {
  n_cols = max(input$col)
  input %>%
    arrange(row, col) %>%
    pull(energy) %>%
    matrix(ncol = n_cols, byrow = TRUE)
}
view_grid(test_11b)

self_adjacent <- function(input) {
  adj <- crossing(x = c(1L, 0L, -1L), y = c(1L, 0L, -1L)) %>%
    filter(!(x == 0L & y == 0L))
  input %>%
    crossing(adj) %>%
    mutate(row2 = row + x, col2 = col + y) %>%
    filter(row2 >= 1L, col2 >= 1L, row2 <= max(row), col2 <= max(row)) %>%
    inner_join(input, by = c("row2" = "row", "col2" = "col"), suffix = c("", "2"))
}

self_adjacent(test_11)

flash_cycle<- function(boosted) {
  boosted %>%
    select(row, col, energy, flashed) %>%
    self_adjacent() %>%
    group_by(row, col, energy, flashed) %>% # Only flash this cycle if you didn't already
    summarise(adj_flashes = sum(energy2 > 9L & (!flashed2)), .groups = "drop") %>%
    mutate(
      flashed = flashed | energy > 9L,
      energy = energy + adj_flashes
    )
}

test_11b %>%
  mutate(energy = energy + 1, flashed = FALSE) %>%
  flash_cycle()

flash_step <- function(input) {
  boosted <- mutate(input, energy = energy + 1, flashed = FALSE)
  while(any(boosted$energy > 9L & (!boosted$flashed))) {
    boosted <- flash_cycle(boosted)
  }
  boosted %>%
    mutate(energy = if_else(flashed, 0, energy))
}

flash_step(test_11b)


q11a <- function(input) {
  boosted <- mutate(input, energy = energy + 1, flashed = FALSE)
  while(any(boosted$energy > 9L & (!boosted$flashed))) {
    boosted <- flash_cycle(boosted)
  }
  boosted %>%
    mutate(energy = if_else(flashed, 0, energy))
}

q11a(test_11b) %>% view_grid()
