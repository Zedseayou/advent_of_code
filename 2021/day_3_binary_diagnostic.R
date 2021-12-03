library(tidyverse)

input_3 <- read_lines("2021/data/input_3.txt") %>%
  enframe(name = NULL) %>%
  separate(value, into = as.character(1:12), sep = "(?<=.)")

test_3 <- read_lines("00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
") %>%
  enframe(name = NULL) %>%
  separate(value, into = as.character(1:5), sep = "(?<=.)")

bin_to_int <- function(bin) {
  bits <- bin %>%
    str_split("") %>%
    pluck(1) %>%
    as.integer()
  powers <- rev(seq_along(bits)) - 1
  as.integer(sum(bits * 2^powers))
}
bin_to_int("101010") # 42

most_least_common <- function(x) {
  counts <- table(x)
  names(counts)[c(which.max(counts), which.min(counts))]
}
most_least_common(c("1", "0", "1", "0", "0")) # c("0", "1")
most_least_common(c("1", "0", "1", "0")) # c("0", "0")

q3a <- function(input) {
  rates <- input %>%
    map(most_least_common) %>%
    transpose(.names = c("gamma", "epsilon")) %>%
    map(~ str_c(.x, collapse = "") %>% bin_to_int)
  prod(as.integer(rates))
}

q3a(input_3)

support_rating <- function(input, support = c("o2", "co2")) {
  index_to_keep <- function(bit_vals, support) {
    mlcv <- most_least_common(bit_vals)
    if (support == "o2") {
      if (mlcv[1] == mlcv[2]) {
        keep_digit <- "1"
      } else {
        keep_digit <- mlcv[1]
      }
    } else if (support == "co2") {
      if (mlcv[1] == mlcv[2]) {
        keep_digit <- "0"
      } else {
        keep_digit <- mlcv[2]
      }
    }
    bit_vals == keep_digit
  }

  working <- input
  for (bit_num in seq_along(working)) {
    bit_vals <- working[[bit_num]]
    working <- working[index_to_keep(bit_vals, support), ]
    if (nrow(working) == 1) {
      break
    }
  }
  working %>%
    str_c(collapse = "") %>%
    bin_to_int()
}

support_rating(test_3, "o2")
support_rating(test_3, "co2")

q3b <- function(input) {
  o2_rating <- support_rating(input, "o2")
  co2_rating <- support_rating(input, "co2")
  o2_rating * co2_rating
}

q3b(input_3)
