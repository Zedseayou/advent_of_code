library(tidyverse)
library(bit64)

test_06 <- c(3,4,3,1,2)
input_06 <- read_file("2021/data/input_06.txt") %>%
  str_trim() %>%
  str_split(",") %>%
  `[[`(1) %>%
  as.integer()

lanternfish_cycle <- function(ages, day) {
  new_ages <- ages - 1
  new_fish <- new_ages < 0
  new_ages[new_fish] <- 6
  c(new_ages, rep(8, sum(new_fish)))
}

q6a <- function(input, n_days) {
  final_fish <- reduce(1:n_days, lanternfish_cycle, .init = input)
  length(final_fish)
}

q6a(test_06, 80)
q6a(input_06, 80)

lanternfish_cycle_2 <- function(n_fish, day) {
  spawning <- n_fish[1]
  n_fish[1:8] <- n_fish[2:9]
  n_fish[7] <- n_fish[7] + spawning
  n_fish[9] <- spawning
  n_fish
}

lanternfish_cycle_2(age_counts, 1)

q6b <- function(input, n_days) {
  age_counts <- input %>%
    table() %>%
    as.integer() %>%
    as.integer64() %>%
    enframe(name = "fish_counter", value = "n_fish") %>%
    complete(fish_counter = 0:8, fill = list(n_fish = 0L)) %>%
    deframe()

  final_fish <- reduce(1:n_days, lanternfish_cycle_2, .init = age_counts)
  sum(final_fish)
}

q6b(test_06, 256)
q6b(input_06, 256)
