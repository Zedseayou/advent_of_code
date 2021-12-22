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
    as.matrix() %>%
    split(f = 1:nrow(.))
}

test_22a <- parse_input("2021/data/test_22a.txt")
test_22b <- parse_input("2021/data/test_22b.txt")
input_22 <- parse_input("2021/data/input_22.txt")

initial <- tibble(x = integer(), y = integer(), z = integer())

do_step <- function(grid, step) {
  c(xmin, xmax, ymin, ymax, zmin, zmax, to_state) %<-% step
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
  min <- c(box1[1], box2[1], box1[3], box2[3], box1[5], box2[5])
  max <- c(box1[2], box2[2], box1[4], box2[4], box1[6], box2[6])
  if (any(min > max)) {
    out <- NULL
  } else {
    out <- c(
      max(box1[1], box2[1]), # xmin
      min(box1[2], box2[2]), # xmax
      max(box1[3], box2[3]), # ymin
      min(box1[4], box2[4]), # ymax
      max(box1[5], box2[5]), # zmin
      min(box1[6], box2[6]),  # zmax
      NA_real_
    )
  }
  out
}

box_intersection(test_22a[[1]], test_22a[[2]])

bench::mark(
  box_volume(test_22a[[1]]),
  box_volume2(test_22a[[1]])
)

box_volume <- function(box) {
  # xmin, xmax, ymin, ymax, zmin, zmax, state
  c(box[2] - box[1] + 1) * c(box[4] - box[3] + 1) * c(box[6] - box[5] + 1) * box[7]
}

box_volume(test_22a[[1]])

# For each new box, calculate intersection with lit boxes and previous overlaps
# New intersection should be opposite sign of previous
q22b <- function(step_list) {
  box_list <- list()
  pb <- progress_bar$new(
    format = "  calculating [:bar] :current/:total (:percent) in :elapsed, eta :eta",
    total = length(step_list),
    clear = FALSE
  )
  pb$tick(0)

  for (step in step_list) {
    for (box in box_list) {
      intersection <- box_intersection(box, step)
      if (!is.null(intersection)) {
        intersection[7] <- -1 * box[7]
        box_list <- append(box_list, list(intersection))
        }
    }
    if (step[7] == 1) {
      box_list <- append(box_list, list(step))
    }
    pb$tick(1)
  }
  box_list %>%
    map_dbl(box_volume) %>%
    sum()
}

q22b(test_22a)
q22b(test_22b[1:20])
part_b_result2 <- q22b(input_22)
part_b_result <- q22b(input_22)
as.character(part_b_result)
