library(tidyverse)

parse_input <- function(raw) {
  lines <- read_lines(raw)

  template <- str_split(lines[1], "")[[1]]
  rules <- lines[3:length(lines)] %>%
    enframe(name = NULL) %>%
    separate(value, c("pair", "insertion"), sep = " -> ")

  list(template = template, rules = rules)
}

test_14 <- parse_input("NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

input_14 <- parse_input("2021/data/input_14.txt")

to_pairs <- function(template) {
  template %>%
    enframe(name = NULL, value = "letter") %>%
    mutate(
      next_letter = lead(letter, default = ""),
      pair = str_c(letter, next_letter)
    )
}

to_pairs(test_14$template)

insert_step <- function(template, rules) {
  pairs <- to_pairs(template)
  pairs %>%
    left_join(rules, by = "pair") %>%
    mutate(
      insertion = replace_na(insertion, ""),
      new = str_c(letter, insertion)
    ) %>%
    pull(new) %>%
    str_split("") %>%
    flatten_chr()
}

insert_step(test_14$template, test_14$rules)

q14a <- function(input, n_steps) {
  insert_with_rules <- function(x) insert_step(x, input$rules)
  fun_list <- rep(list(insert_with_rules), times = n_steps)
  polymer <- compose(!!!fun_list)(input$template)
  counts <- table(polymer)
  # counts
  max(counts) - min(counts)
}

q14a(test_14, 10)
q14a(input_14, 10)

insert_step_counts <- function(pairs, rules) {
  new_pairs <- pairs %>%
    left_join(rules, by = "pair") %>%
    mutate(split = str_split(pair, "")) %>%
    hoist(split, p1 = 1, p2 = 2) %>%
    mutate(p1 = str_c(p1, insertion), p2 = str_c(insertion, p2))

  unchanged <- new_pairs %>%
    filter(is.na(insertion)) %>%
    select(pair, n)
  pair_1 <- select(new_pairs, pair = p1, n)
  pair_2 <- select(new_pairs, pair = p2, n)

  bind_rows(unchanged, pair_1, pair_2) %>%
    group_by(pair) %>%
    summarise(n = sum(n), .groups = "drop")
}

test_pairs <- test_14$template %>%
  to_pairs() %>%
  filter(str_length(pair) == 2) %>%
  count(pair)

insert_step_counts(test_pairs, test_14$rules)

q14b <- function(input, n_steps) {
  pair_counts <- input$template %>%
    to_pairs() %>%
    filter(str_length(pair) == 2) %>%
    count(pair)

  insert_with_rules <- function(x) insert_step_counts(x, input$rules)
  fun_list <- rep(list(insert_with_rules), times = n_steps)
  polymer_pairs <- compose(!!!fun_list)(pair_counts)
  letter_counts <- polymer_pairs %>%
    mutate(split = str_split(pair, "")) %>%
    unnest(split) %>%
    group_by(split) %>%
    summarise(n = ceiling(sum(n) / 2)) # letter in a pair is 2 appearances except at ends
  # letter_counts
  max(letter_counts$n) - min(letter_counts$n)
}

q14b(test_14, 10)
q14b(input_14, 10)
q14b(input_14, 40) %>% as.character()
