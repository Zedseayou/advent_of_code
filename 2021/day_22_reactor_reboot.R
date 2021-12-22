library(tidyverse)
library(zeallot)
library(progress)
library(sf)

parse_input <- function(raw) {
  raw %>%
    read_lines() %>%
    enframe(name = NULL) %>%
    extract(
      col = value,
      into = c("on_off", "xmin", "xmax", "ymin", "ymax", "zmin", "zmax"),
      regex = "(on|off) x=([-\\d]+)..([-\\d]+),y=([-\\d]+)..([-\\d]+),z=([-\\d]+)..([-\\d]+)"
    ) %>%
    mutate(
      state = if_else(on_off == "on", 1L, 0L),
      across(c(xmin:zmax), as.numeric)
    ) %>%
    rowwise() %>%
    group_split()
}

test_22a <- parse_input("2021/data/test_22a.txt")
test_22b <- parse_input("2021/data/test_22b.txt")
input_22 <- parse_input("2021/data/input_22.txt")

initial <- tibble(x = integer(), y = integer(), z = integer())

do_step <- function(grid, instructions) {
  c(on_off, xmin, xmax, ymin, ymax, zmin, zmax, to_state) %<-% instructions
  new_set <- crossing(x = xmin:xmax, y = ymin:ymax, z = zmin:zmax, state = to_state)
  grid %>%
    anti_join(new_set, by = c("x", "y", "z")) %>%
    bind_rows(new_set)
}

do_step(initial, test_22a[[1]])
do_step(initial, test_22a[[1]]) %>% do_step(test_22a[[2]]) %>% do_step(test_22a[[3]]) %>% view()

q22a <- function(step_list) {
  pb <- progress_bar$new(
    format = "  calculating [:bar] :current/:total (:percent) in :elapsed, eta :eta",
    total = length(step_list),
    clear = FALSE
  )
  grid <- initial

  pb$tick(0)
  for (step in step_list) {
    grid <- do_step(grid, step)
    pb$tick()
  }
  sum(grid$state)
}

q22a(test_22a)
q22a(test_22b[1:20])
q22a(input_22[1:20])
