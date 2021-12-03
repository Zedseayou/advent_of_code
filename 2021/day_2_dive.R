library(tidyverse)

input_2 <- read_table2("2021/data/input_2.txt", col_names = c("direction", "distance"))

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

q2a(input_2)


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

q2b(input_2)
