library(tidyverse)

input_02 <- read_table2("2021/data/input_02.txt", col_names = c("direction", "distance"))

q2a <- function(input){
  position <- input %>%
    mutate(
      dx = if_else(direction == "forward", distance, 0),
      dy = case_when(
        direction == "up" ~ -distance,
        direction == "down" ~ distance,
        TRUE ~ 0
      )
    ) %>%
    summarise(x = sum(dx), y = sum(dy))
  position$x * position$y
}

q2a(input_02)


q2b <- function(input) {
  position <- input %>%
    mutate(
      da = case_when(
        direction == "up" ~ - distance,
        direction == "down" ~ distance,
        TRUE ~ 0
      ),
      aim = cumsum(da),
      dx = if_else(direction == "forward", distance, 0),
      dy = if_else(direction == "forward", distance * aim, 0)
    ) %>%
    summarise(x = sum(dx), y = sum(dy))
  position$x * position$y
}

q2b(input_02)

q2_path <- function(input, part){
  if (part == "a") {
    position <- input %>%
      mutate(
        dx = if_else(direction == "forward", distance, 0),
        dy = case_when(
          direction == "up" ~ -distance,
          direction == "down" ~ distance,
          TRUE ~ 0
        ),
        x = cumsum(dx),
        y = cumsum(dy)
      )
  } else if (part == "b") {
    position <- input %>%
      mutate(
        da = case_when(
          direction == "up" ~ - distance,
          direction == "down" ~ distance,
          TRUE ~ 0
        ),
        aim = cumsum(da),
        dx = if_else(direction == "forward", distance, 0),
        dy = if_else(direction == "forward", distance * aim, 0),
        x = cumsum(dx),
        y = cumsum(dy)
      )
  }
  position %>%
    ggplot(aes(x, y)) +
    geom_line() +
    scale_y_reverse() +
    theme_minimal()
}

q2_path(input_02, "a")
q2_path(input_02, "b")
