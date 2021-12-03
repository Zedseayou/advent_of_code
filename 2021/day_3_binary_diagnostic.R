library(tidyverse)

input_3 <- read_lines("2021/data/input_3.txt") %>%
  enframe(name = NULL) %>%
  separate(value, into = as.character(1:12), sep = "(?<=.)")

bin_to_int <- function(bin) {
  bits <- bin %>%
    str_split("") %>%
    pluck(1) %>%
    as.integer()
  powers <- (length(bits) - 1):0
  sum(bits * 2^powers)
}



mode <- function(x, index_fun) {
  counts <- table(x)
  names(counts)[index_fun(counts)]
}

q5 <- function(input) {
  rate <- function(bits, index_fun) {
    bits %>%
      map(mode, index_fun = index_fun) %>%
      str_c(collapse = "") %>%
      bin_to_int()
  }

  gamma <- rate(input, which.max)
  epsilon <- rate(input, which.min)
  gamma * epsilon
}

q5(input_3)

rating <- function(input, index_fun) {
  indices <- rep(TRUE, nrow(input))
  current <- input
  for (bit in 1:length(input)) {
    current <- filter(current[bit] == mode(current[bit], index_fun))
  }
  current
}

current

q6 <- function(input) {
  o2_indices <- rep(TRUE, nrow(input))
  co2_indices <- o2_indices
  for (bit in 1:length(input)) {
    bit_vals <- input[bit]
    bit_vals == mode()
  }


  o2_step <- function(i, x) x <- x[i]; x == mode(x, which.max)
  co2_step <- function(i, x) x <- x[i]; x == mode(x, which.min)

  reduce2(input, o2_step)
}

q6(input_3)
