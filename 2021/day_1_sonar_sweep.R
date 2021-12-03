library(tidyverse)
library(slider)

input_1 <- read_lines("2021/data/input_1.txt") %>%
  as.integer()

q1a <- function(input) {
  increased <- input > lag(input)
  sum(increased, na.rm = TRUE)
}
q1a(input_1)

q1b <- function(input) {
  window_sums <- slide_int(input, sum, .after = 2, .step = 1, .complete = TRUE)
  increased <- window_sums > lag(window_sums)
  sum(increased, na.rm = TRUE)
}
q1b(input_1)
