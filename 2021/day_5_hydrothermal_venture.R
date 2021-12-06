library(tidyverse)
# messy - to clean up
parse_input <- . %>%
  enframe(name = NULL) %>%
  extract(value, c("x1", "y1", "x2", "y2"), "(\\d+),(\\d+) -> (\\d+),(\\d+)") %>%
  mutate(
    across(.fns = as.integer),
    x = map2(x1, x2, seq),
    y = map2(y1, y2, seq)
  )

input_5 <- read_lines("2021/data/input_5.txt") %>% parse_input

test_5 <- read_lines("0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2") %>% parse_input()

make_grid <- function(input) {
  matrix(
    nrow = max(c(input$y1, input$y2)) - min(c(input$y1, input$y2)),
    ncol = max(c(input$x1, input$x2)) - min(c(input$x1, input$x2))
  )
}
make_grid(test_5)

is_point_on_segment <- function(x, y, x1, y1, x2, y2) {
  if (x2 == x1) {
    out <- x == x1 & y >= min(y1, y2) & y <= max(y1, y2)
  } else if (y2 == y1) {
    out <- y == y1 & x >= min(x1, x2) & x <= max(x1, x2)
  } else {
    m <- (y2 - y1) / (x2 - x1)
    out <- (y - y1) == m * (x - x1)
  }
  out
}
is_point_on_segment(7, 4, 3, 4, 1, 4)

is_point_in_interval <- function(x, x1, x2) {
  x >= pmin(x1, x2) & x <= pmax(x1, x2)
}
is_point_in_interval(c(1, 2), c(3, 4), c(5, 1))

test_5 %>%
  filter(x1 == x2 | y1 == y2) %>%
  mutate(
    x = map2(x1, x2, seq),
    y = map2(y1, y2, seq)
  ) %>%
  unnest(x) %>%
  unnest(y)

line_intersection <- function(x1, y1, x2, y2, x3, y3, x4, y4) {

}

q5a <- function(input) {
  xmin <- min(c(input$x1, input$x2))
  xmax <- max(c(input$x1, input$x2))
  ymin <- min(c(input$y1, input$y2))
  ymax <- max(c(input$y1, input$y2))

  segments <- filter(input, (x1 == x2) | (y1 == y2))
  crossing(x = xmin:xmax, y = ymin:ymax, segments) %>%
    filter(x >= min(x1, x2), x <= max(x1, x2), y >= min(y1, y2), y <= max(y1, y2)) %>%
    mutate(
      x_cross = x1 == x2 & x == x1 & y >= pmin(y1, y2) & y <= pmax(y1, y2),
      y_cross = y1 == y2 & y == y1 & x >= pmin(x1, x2) & x <= pmax(x1, x2),
      is_crossed = x_cross | y_cross
    ) %>%
    # mutate(is_crossed = pmap_lgl(., is_point_on_segment)) %>%
    # filter(x == 7)
    group_by(x, y) %>%
    summarise(n_crossings = sum(is_crossed), .groups = "drop") %>%
    arrange(x, y)
}

q5a <- function(input) {
  input %>%
    filter(x1 == x2 | y1 == y2) %>%
    unnest(x) %>%
    unnest(y) %>%
    count(x, y) %>%
    count(n > 1)
}
q5a(test_5)
q5a(input_5)

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

q5b(test_5)
q5b(input_5)

grid_view <- function(points) {
  matrix(
    data = points$n_crossings,
    nrow = max(points$y) - min(points$y) + 1,
    byrow = FALSE,
    dimnames = list(min(points$x):max(points$x), min(points$y):max(points$y))
  )
}

q5a(test_5) %>% grid_view
q5a(test_5) %>% count(n_crossings > 1)

q5a(input_5) %>% count(n_crossings > 1)
