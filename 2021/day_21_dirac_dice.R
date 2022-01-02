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
    position = starts,
    score = 0,
    n_universes = 1
  )

  roll <- function(states, turn) {
    states %>%
      crossing(roll_odds) %>%
      mutate(
        position = (position + move) %% 10,
        position = if_else(position == 0, 10, position),
        score = score + position,
        n_universes = n_universes * odds
      ) %>%
      group_by(position, score) %>%
      summarise(n_universes = sum(n_universes), .groups = "drop")
  }

  calc_turns <- function(start) {
    # Max turns definitely less than 21 (if you scored 1 every time, which you can't)
    accumulate(1:12, roll, .init = start) %>%
      bind_rows(.id = "turn") %>%
      mutate(turn = as.integer(turn)) %>%
      arrange(turn) %>%
      group_by(turn) %>%
      summarise( # avoid double counting - only count if you reached 21 that turn
        n_could_win = sum(n_universes[score >= 21 & score - position < 21]),
        n_not_won = sum(n_universes[score < 21])
      )
  }

  p1_counts <- calc_turns(slice(states, 1))
  p2_counts <- calc_turns(slice(states, 2))

  p1_counts %>%
    inner_join(p2_counts, by = "turn", suffix = c("_1", "_2")) %>%
    mutate(
      n_p1_wins = n_could_win_1 * lag(n_not_won_2, default = 0),
      n_p2_wins = n_could_win_2 * n_not_won_1
    ) %>%
    summarise(across(ends_with("wins"), ~as.character(sum(.x))))
}

q21b(test_21)
q21b(input_21)

# Does player 1 always win?
combos <- combn(1:10, 2, simplify = FALSE)
combos <- append(combos, map(combos, rev))
all_games <- map_dfr(combos, ~ q21b(.x) %>% mutate(p1 = .x[1], p2 = .x[2]))
all_games %>%
  mutate(n_p1_wins = as.numeric(n_p1_wins), n_p2_wins = as.numeric(n_p2_wins)) %>%
  count(n_p1_wins > n_p2_wins) # p2 wins only 6 possible starting situations
all_games %>%
  mutate(n_p1_wins = as.numeric(n_p1_wins), n_p2_wins = as.numeric(n_p2_wins)) %>%
  filter(n_p2_wins > n_p1_wins) # only happens when p2 starts on 1 and p1 starts on 3-8
