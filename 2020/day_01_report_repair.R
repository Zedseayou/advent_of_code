library(tidyverse)

input_01 <- read_csv("2020/data/input_01.txt", col_names = FALSE)

q1a <- function(input) {
  combos <- combn(x = input$X1, m = 2)
  index <- which(combos[1,] + combos[2,] == 2020)
  prod(combos[, index])
}

q1a(input_1)

q1b <- function(input) {
  combos <- combn(x = input$X1, m = 3)
  index <- which(colSums(combos) == 2020)
  prod(combos[, index])
}

q1b(input_01)
