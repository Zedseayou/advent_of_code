library(tidyverse)

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

test_25 <- parse_input("2021/data/test_25.txt")
input_25 <- parse_input("2021/data/input_25.txt")

move_right <- function(input) {
  input %>%
    arrange(row, col) %>%
    group_by(row) %>%
    mutate(
      first_cell = first(cell),
      last_cell = last(cell),
      right_cell = lead(cell) %>% coalesce(first_cell),
      left_cell = lag(cell) %>% coalesce(last_cell)
    ) %>%
    ungroup() %>%
    transmute(
      row,
      col,
      cell = case_when(
        cell == ">" & right_cell == "." ~ ".",
        cell == "." & left_cell == ">" ~ ">",
        TRUE ~ cell
      )
    )
}

bench::mark(
  move_right(test_25),
  move_right2(test_25)
)

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
    scale_fill_manual(values = c("#bdbdbd", "#ef8a62", "#67a9cf")) +
    theme_void()
}

plot_cucumber(test_25)
plot_cucumber(move_right(test_25))
plot_cucumber(input_25)

q25a <- function(input) {
  old <- input
  i <- 0
  repeat {
    new <- old %>% move_right() %>% move_down()
    i <- i + 1
    if (identical(new, old)) {
      break
    }
    old <- new
  }
  list(i, new)
}
