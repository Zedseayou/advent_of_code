library(tidyverse)

input_02 <- read_delim(
  file = "2020/data/input_02.txt",
  delim = " ",
  col_names = c("number", "letter", "password")
)


q2a <- function(input) {
  clean <- input %>%
    separate(number, c("min", "max"), sep = "-") %>%
    mutate(
      min = as.integer(min),
      max = as.integer(max),
      letter = str_remove_all(letter, ":")
    )

  validated <- clean %>%
    mutate(letters = map(password, ~ str_split(.x, "")[[1]])) %>%
    unnest_longer(letters) %>%
    count(min, max, letter, password, letters) %>%
    ungroup() %>%
    filter(letter == letters) %>% # if letter doesn't appear, it's invalid (min >= 1)
    mutate(valid = n >= min & n <= max)
  sum(validated$valid)
}

q2a(input_02)

q2b <- function(input) {
  clean <- input %>%
    separate(number, c("pos1", "pos2"), sep = "-") %>%
    mutate(
      pos1 = as.integer(pos1),
      pos2 = as.integer(pos2),
      letter = str_remove_all(letter, ":")
    )

  validated <- clean %>%
    mutate(letters = map(password, ~ str_split(.x, "")[[1]])) %>%
    unnest_longer(letters, indices_to = "position") %>%
    filter(pos1 == position | pos2 == position) %>%
    mutate(match = letter == letters) %>%
    group_by(password) %>%
    filter(sum(match) == 1)
  sum(validated$match)
}

q2b(input_02)
