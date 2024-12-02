# Solution to https://adventofcode.com/2021/day/25

library(tidyverse)

# Input provided as text grid: parse each value into row-col indexed dataframe

parse_input <- function(raw) {
  raw %>%
    read_lines %>%
    enframe(name = "row") %>%
    mutate(
      cell = str_split(value, ""),
      col = map(cell, ~ 1:length(.x))
    ) %>%
    select(-value) %>%
    unnest(c(col, cell))
}

# These can be downloaded for replication from the linked website

test_25 <- parse_input("2021/data/test_25.txt")
input_25 <- parse_input("2021/data/input_25.txt")

# For each row, cucumbers can only move if the cell to move into is empty

move_right <- function(input) {
  input %>%
    arrange(row, col) %>%
    group_by(row) %>%
    mutate(
      first_cell = first(cell),
      last_cell = last(cell),
      right_cell = lead(cell) %>% coalesce(first_cell), # grid wraps around
      left_cell = lag(cell) %>% coalesce(last_cell)
    ) %>%
    ungroup() %>%
    transmute(
      row,
      col,
      cell = case_when(
        cell == ">" & right_cell == "." ~ ".", # Outgoing movement
        cell == "." & left_cell == ">" ~ ">",  # Incoming movement
        TRUE ~ cell
      )
    )
}

move_down<- function(input) {
  input %>%
    arrange(col, row) %>%
    group_by(col) %>%
    mutate(
      first_cell = first(cell),
      last_cell = last(cell),
      next_cell = lead(cell) %>% coalesce(first_cell),
      prev_cell = lag(cell) %>% coalesce(last_cell)
    ) %>%
    ungroup() %>%
    transmute(
      row,
      col,
      cell = case_when(
        cell == "v" & next_cell == "." ~ ".",
        cell == "." & prev_cell == "v" ~ "v",
        TRUE ~ cell
      )
    )
}

# Testing to ensure this implementation is adequately fast
bench::mark(
  move_right(test_25),
  move_right(input_25),
  move_down(test_25),
  check = FALSE
)


plot_cucumber <- function(grid) {
  grid %>%
    ggplot() +
    geom_raster(aes(x = col, y = row, fill = cell)) +
    scale_y_reverse() +
    scale_fill_manual(values = c("#bdbdbd", "#ef8a62", "#67a9cf")) +
    theme_void()
}

plot_cucumber(test_25)
plot_cucumber(move_right(test_25))
plot_cucumber(input_25)

# To solve part a, we just iterate the movement until nothing changes

q25a <- function(input) {
  old <- input
  grids <- list(input)
  repeat {
    new <- old %>% move_right() %>% move_down()
    grids <- append(grids, list(new))
    if (identical(new, old)) {
      break
    }
    old <- new
  }
  grids
}

test_out <- q25a(test_25)
input_out <- q25a(input_25)
length(input_out) - 1

# And we can even make a gif of how everything moves!

library(gganimate)

test_gif <- test_out %>%
  bind_rows(.id = "step") %>%
  mutate(step = as.integer(step) - 1) %>%
  plot_cucumber() +
  transition_manual(frames = step) +
  ggtitle('Now showing step {current_frame} of {nframes - 1}')

animate(test_gif, nframes = 59, fps = 5, start_pause = 10, end_pause = 10)

input_gif <- input_out %>%
  bind_rows(.id = "step") %>%
  mutate(step = as.integer(step) - 1) %>%
  plot_cucumber() +
  transition_manual(frames = step) +
  ggtitle('Now showing step {current_frame} of {nframes - 1}')

animate(input_gif, nframes = 529, fps = 10, start_pause = 10, end_pause = 10)
