library(tidyverse)
library(zeallot)
library(progress)

parse_input <- function(raw) {
  raw %>%
    read_lines() %>%
    str_split("\\s")
}

test_24a <- parse_input(
"inp x
mul x -1")

test_24b <- parse_input(
"inp z
inp x
mul z 3
eql z x")

test_24c <- parse_input(
"inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2"
)
input_24_lines <- read_lines("2021/data/input_24.txt")
input_24 <- parse_input("2021/data/input_24.txt")

inp_rows <- input_24 %>%
  map_lgl(~ "inp" %in% .x) %>%
  which()

get_b <- function(var, vars) {
  if (var %in% c("w", "x", "y", "z")) {
    out <- vars[var]
  } else {
    out <- as.integer(var)
  }
  out
}

alu <- function(variables, instruction) {
  c(op, a, b = NULL) %<-% instruction
  if (op == "inp") {
    variables[a] <- get_b(b, variables)
  } else if (op == "add") {
    variables[a] <- variables[a] + get_b(b, variables)
  } else if (op == "mul") {
    variables[a] <- variables[a] * get_b(b, variables)
  } else if (op == "div") {
    quotient <- variables[a] / get_b(b, variables)
    variables[a] <- if_else(quotient >= 0, floor(quotient), ceiling(quotient))
  } else if (op == "mod") {
    variables[a] <- variables[a] %% get_b(b, variables)
  } else if (op == "eql") {
    variables[a] <- as.integer(variables[a] == get_b(b, variables))
  }
  variables
}

test_alu <- function(program, input) {
  program[[1]] <- c(program[[1]], input)
  v_init <- c(w = 0, x = 0, y = 0, z = 0)
  reduce(program, .f = alu, .init = v_init)
}
test_alu(test_24a, "5")
test_alu(test_24c, "6")
test_alu(test_24c, "9")


monad <- function(program, model_number) {
  digits <- str_split(as.character(model_number), "")[[1]]
  if ("0" %in% digits) {
    return(c(w = 0, x = 0, y = 0, z = 1))
  }

  program[inp_rows] <- map2(program[inp_rows], digits, ~c(.x, .y))
  v_init <- c(w = 0, x = 0, y = 0, z = 0)
  reduce(program, .f = alu, .init = v_init)
}

monad(input_24, 13579246899999)

q22a_slow <- function(program) {
  n = 99999999999999
  valid = FALSE
  pb <- progress_bar$new(
    format = "  calculating :current in :elapsed",
    total = NA,
    clear = FALSE
  )
  pb$tick(0)


  while(!valid) {
    vars <- monad(program, n)
    valid <- vars["z"] == 0
    n <- n - 1
    pb$tick()
  }
  n + 1
}
# Too slow
q22a(input_24)


test_alu(input_24[1:18], "4")

# Try to see differences in chunks
chunks <- input_24_lines %>%
  enframe(name = NULL) %>%
  mutate(
    chunk_id = rep(1:14, each = 18),
    instruction_id = rep(1:18, times = 14)
  )

# Solved based on this description:
# https://old.reddit.com/r/adventofcode/comments/rnejv5/2021_day_24_solutions/hps5hgw/
# Code in each chunk translates to:
# w <- input
# x <- ((z %% 26) + b) != w
# z <- z / a
# z <- z * ((25 * x) + 1)
# z <- z + ((w + c) * x)
chunks %>%
  spread(chunk_id, value) %>%
  view()

# On looking at the chunks, a is either 1 or 26.
# When a is 1, b is between 10 and 16, so x is always 1
# (because the first part of the line can't be a single digit number)
# This makes the code read:
# z <- z * 26
# z <- z + (w + c)
# which is strictly increasing z (adding a digit in base 26).
# So we need to use the chunks with z <- z / 26 to remove digits, and avoid any
# added digits by setting x to zero in each step.
# This only happens if the last digit of z (z %% 26) w_push + c_push = w_pop - b_pop
chunks %>%
  filter(instruction_id %in% c(5, 6, 16)) %>%
  mutate(
    instruction_id = case_when(
      instruction_id == 5 ~ "a",
      instruction_id == 6 ~ "b",
      instruction_id == 16 ~ "c"
    ),
    value = str_extract(value, "(\\d|-)+$") %>% as.integer()
  ) %>%
  spread(instruction_id, value) %>%
  mutate(difference = b + c)

# The digits all pair off according to which chunk is popping which chunk pushed
# This makes it easy to figure out what the highest and lowest valid numbers are
monad(input_24, "99893999291967")
monad(input_24, "34171911181211")
