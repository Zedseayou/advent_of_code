library(tidyverse)

input_2 <- read_table2("2021/data/input_2.txt", col_names = c("direction", "distance"))

q3 <- function(input){
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

q3(input_2)


q4 <- function(input) {
  change_aim <- function(aim, direction, distance) {
    if (direction == "down") {
      aim <- aim + distance
    } else if (direction == "up"){
      aim <- aim - distance
    }
    aim
  }
  aim <- accumulate2(
    .x = input$direction,
    .y = input$distance,
    .f = change_aim,
    .init = 0
  ) %>%
    as.integer()

  position <- input %>%
    mutate(
      aim = aim[-1],
      dx = if_else(direction == "forward", distance, 0),
      dy = if_else(direction == "forward", distance * aim, 0)
    ) %>%
    summarise(x = sum(dx), y = sum(dy))
  position$x * position$y
}

q4(input_2)
