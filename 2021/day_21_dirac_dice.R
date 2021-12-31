library(tidyverse)

test_21 <- c(4, 8)
input_21 <- c(7, 8)

q21a <- function(starts, n_rolls) {
  dirac_game <- tibble(
    rolls = rep(1:100, length.out = n_rolls), # divisible by 6
    roll_number = 1:n_rolls,
    player = if_else(roll_number %% 6 %in% c(1, 2, 3), 1, 2),
    turn = rep(1:(n_rolls %/% 6 + 1), each = 6, length.out = n_rolls)
  ) %>%
    group_by(turn, player) %>%
    summarise(
      move = sum(rolls),
      rolls = list(rolls),
      roll_count = max(roll_number),
      .groups = "drop"
    ) %>%
    group_by(player) %>%
    mutate(
      start = if_else(player == 1, starts[1], starts[2]),
      score = (cumsum(move) + start) %% 10,
      score = if_else(score == 0, 10, score),
      total_score = cumsum(score)
    ) %>%
    ungroup()

  if (max(dirac_game$total_score) < 1000) {
    stop("Didn't reach target, use longer n_rolls")
  }
  winner <- which.max(dirac_game$total_score >= 1000)
  last_turns <- slice(dirac_game, (winner - 1):winner)

  last_turns$roll_count[2] * last_turns$total_score[1]
}
q21a(test_21, 1000)
q21a(input_21, 1000)

# Each turn:

q21b <- function(starts) {
  roll_odds <- tibble(
    move = 3:9,
    odds = c(1, 3, 6, 7, 6, 3, 1)
  )
  states <- tibble(
    player = 1:2,
    position = starts,
    score = 0,
    n_universes = 1
  )

  roll <- function(states, player) {
    if (player == 1) {
      out <- states %>%
        crossing(roll_odds) %>%
        mutate(
          position_1 = (position_1 + move) %% 10,
          position_1 = if_else(position_1 == 0, 10, position_1),
          score_1 = score_1 + position_1,
          n_universes = n_universes * odds
        ) %>%
        select(n_universes, position_1, position_2, score_1, score_2)
    } else if (player == 2) {
      out <- states %>%
        crossing(roll_odds) %>%
        mutate(
          position_2 = (position_2 + move) %% 10,
          position_2 = if_else(position_2 == 0, 10, position_2),
          score_2 = score_2 + position_2,
          n_universes = n_universes * odds
        ) %>%
        select(n_universes, position_1, position_2, score_1, score_2)
    }
    out
  }
  roll(states, 1) %>%
    roll(2) %>%
    roll(1)

  completed <- tibble()
  i <- 0
  while (!is.null(states)) {
    # browser()
    states <- roll(states, 1)
    states_split <- split(states, states$score_1 >= 21)
    completed <- bind_rows(completed, states_split[["TRUE"]])
    states <- states_split[["FALSE"]]
    if (is.null(states)) {
      break
    }
    # browser()
    states <- roll(states, 2)
    states_split <- split(states, states$score_2 >= 21)
    completed <- bind_rows(completed, states_split[["TRUE"]])
    states <- states_split[["FALSE"]]
    print(i)
    i <- i + 1
  }
  completed %>%
    group_by(p1_win = score_1 >= 21, p2_win = score_2 >= 21) %>%
    summarise(n_universes = sum(n_universes) %>% as.character)
}

q21b(test_21)
q21b(input_21)
