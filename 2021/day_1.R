library(tidyverse)
library(slider)

input_1 <- read_lines("2021/data/input_1.txt")

q1 <- function(input) {
  depths <- as.integer(input)
  increased <- depths > lag(depths)
  sum(increased, na.rm = TRUE)
}
q1(input_1)

q2 <- function(input) {
  depths <- as.integer(input)
  window_sums <- slide_int(depths, sum, .after = 2, .step = 1, .complete = TRUE)
  increased <- window_sums > lag(window_sums)
  sum(increased, na.rm = TRUE)
}
q2(input_1)
