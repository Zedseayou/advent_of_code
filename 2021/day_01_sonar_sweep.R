library(tidyverse)
library(slider)

input_01 <- read_lines("2021/data/input_0\1.txt") %>%
  as.integer()

q1a <- function(input) {
  increased <- input > lag(input)
  sum(increased, na.rm = TRUE)
}
q1a(input_01)

q1b <- function(input) {
  window_sums <- slide_int(input, sum, .after = 2, .step = 1, .complete = TRUE)
  increased <- window_sums > lag(window_sums)
  sum(increased, na.rm = TRUE)
}
q1b(input_01)
