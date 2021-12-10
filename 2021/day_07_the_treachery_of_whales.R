library(tidyverse)

parse_input <- function(input) {
  input %>%
    str_split(",") %>%
    `[[`(1) %>%
    as.integer()
}

input_07 <- read_file("2021/data/input_07.txt") %>% parse_input
test_07 <- "16,1,2,0,4,2,7,1,2,14" %>% parse_input

cost_of_level <- function(level, input) {
  sum(abs(input - level))
}

q7a <- function(input) {
  fuel_cost <- function(level, input) sum(abs(input - level))
  costs <- map_int(min(input):max(input), fuel_cost, input = input)
  c(position = which.min(costs) - 1, cost = min(costs))
}

q7a(test_07)
q7a(input_07)

q7b <- function(input){
  fuel_cost <- function(level, input) {
    n <- abs(input - level)
    sum(n * (1L + n) / 2L)
  }
  costs <- map_dbl(min(input):max(input), fuel_cost, input = input)
  c(position = which.min(costs) - 1, cost = min(costs))
}

q7b(test_07)
q7b(input_07)
