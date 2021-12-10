library(tidyverse)

parse_input <- . %>%
  enframe(name = NULL) %>%
  extract(value, c("x1", "y1", "x2", "y2"), "(\\d+),(\\d+) -> (\\d+),(\\d+)") %>%
  mutate(
    across(.fns = as.integer),
    x = map2(x1, x2, seq),
    y = map2(y1, y2, seq)
  )

input_05 <- read_lines("2021/data/input_05.txt") %>% parse_input

test_05 <- read_lines("0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2") %>% parse_input()

q5a <- function(input) {
  input %>%
    filter(x1 == x2 | y1 == y2) %>%
    unnest(x) %>%
    unnest(y) %>%
    count(x, y) %>%
    count(n > 1)
}
q5a(test_05)
q5a(input_05)

q5b <- function(input) {
  hv <- input %>%
    filter(x1 == x2 | y1 == y2) %>%
    unnest(x) %>%
    unnest(y)

  diag <- input %>%
    filter(!(x1 == x2 | y1 == y2)) %>%
    unnest(c(x, y))

  bind_rows(hv, diag) %>%
    count(x, y) %>%
    count(n > 1)
}

q5b(test_05)
q5b(input_05)
