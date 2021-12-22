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
    select(-on_off) %>%
    rowwise() %>%
    group_split()
}

test_22a <- parse_input("2021/data/test_22a.txt")
test_22b <- parse_input("2021/data/test_22b.txt")
input_22 <- parse_input("2021/data/input_22.txt")

initial <- tibble(x = integer(), y = integer(), z = integer())

do_step <- function(grid, instructions) {
  c(xmin, xmax, ymin, ymax, zmin, zmax, to_state) %<-% instructions
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

box_intersection <- function(box1, box2) {
  axis_bound_pairs <- tibble(
    min = c(box1$xmin, box2$xmin, box1$ymin, box2$ymin, box1$zmin, box2$zmin),
    max = c(box2$xmax, box1$xmax, box2$ymax, box1$ymax, box2$zmax, box1$zmax),
    conflict = min > max
  )
  if (any(axis_bound_pairs$conflict)) {
    out <- NULL
  } else {
    out <- tibble_row(
      xmin = max(box1$xmin, box2$xmin),
      xmax = min(box1$xmax, box2$xmax),
      ymin = max(box1$ymin, box2$ymin),
      ymax = min(box1$ymax, box2$ymax),
      zmin = max(box1$zmin, box2$zmin),
      zmax = min(box1$zmax, box2$zmax),
    )
  }
  out
}

box_intersection(test_22a[[1]], test_22a[[2]])

box_volume <- function(box) {
  c(xmin, xmax, ymin, ymax, zmin, zmax, state) %<-% box
  c(xmax - xmin + 1) * c(ymax - ymin + 1) * c(zmax - zmin + 1) * state
}
box_volume(test_22a[[1]])

# For each step, check the intersection with each lit box
# If there is an intersection, keep the intersection with negative volume
# If the new box turns on, keep it as well
q22b <- function(step_list) {
  box_list <- list()
  overlaps <- list()
  for (step in step_list) {
    for (box in box_list) {
      intersection <- box_intersection(box, step)
      if (!is.null(intersection)) {
        idx <- detect_index(box_list, ~ identical(.x, intersection))
        if (idx == 0) {
          intersection$state <- -1
          box_list <- append(overlaps, list(intersection))
        } else {
          box_list[[idx]]$state <- box_list[[idx]]$state * -1
        }
      }
    }
    if (step$state == 1) {
      box_list <- append(box_list, list(step))
    }
  }
  append(box_list, overlaps) %>%
    map_dbl(box_volume) %>%
    sum()
}

q22b(test_22a)
q22b(test_22b[1:20])

list(
  test_22a[[1]],
  test_22a[[2]],
  box_intersection(test_22a[[1]], test_22a[[2]]) %>% `$<-`(state, -1),
  test_22a[[3]],
  box_intersection(test_22a[[1]], test_22a[[3]]) %>% `$<-`(state, -1),
  box_intersection(test_22a[[2]], test_22a[[3]]) %>% `$<-`(state, -1),
  test_22a[[4]],
  box_intersection(test_22a[[1]], test_22a[[4]]) %>% `$<-`(state, -1),
  box_intersection(test_22a[[2]], test_22a[[4]]),
  box_intersection(test_22a[[3]], test_22a[[4]]) %>% `$<-`(state, -1)
) %>%
  compact() %>%
  map_dbl(box_volume) %>%
  cumsum()

box_intersection(test_22a[[1]], test_22a[[2]])
