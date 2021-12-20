library(tidyverse)
library(zeallot)
library(progress)

sfn_parse <- function(raw) {
  if (length(raw) == 1 && !file.exists(raw)) {
    raw <- str_c(raw, "\n")
  }
  raw %>%
    read_lines() %>%
    str_split("")
}

test_explode <- sfn_parse(
"[[[[[9,8],1],2],3],4]
[7,[6,[5,[4,[3,2]]]]]
[[6,[5,[4,[3,2]]]],1]
[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]
[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")

test_sums <- list(
  "[1,1]
[2,2]
[3,3]
[4,4]",
  "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]",
  "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]",
  "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"
) %>% map(sfn_parse)

test_magnitudes <- sfn_parse(
"[[9,1],[1,9]]
[[1,2],[[3,4],5]]
[[[[0,7],4],[[7,8],[6,0]]],[8,1]]
[[[[1,1],[2,2]],[3,3]],[4,4]]
[[[[3,0],[5,3]],[4,4]],[5,5]]
[[[[5,0],[7,4]],[5,5]],[6,6]]
[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")

test_18 <- sfn_parse(
"[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
)

input_18 <- sfn_parse("2021/data/input_18.txt")

sfn_values <- function(line) {
  level <- cumsum(c(line == "[") - c(line == "]"))
  values_i <- str_which(line, "\\d")
  tibble(
    value = as.integer(line[values_i]),
    level = level[values_i]
  )
}

sfn_explode <- function(sfn) {
  if (all(sfn$level <= 4)) {
    return(sfn)
  } else {
    c(values, levels) %<-% sfn
  }
  # Only the first pair that is too deep will be exploded
  c(l_i, r_i, ...rest = NULL) %<-% which(levels > 4)
  # Add exploded pair values to neighbours if possible
  if (l_i > 1) values[l_i - 1] <- values[l_i - 1] + values[l_i]
  if (r_i < length(values)) values[r_i + 1] <- values[r_i + 1] + values[r_i]
  # Reset where the pair used to be
  values[r_i] <- 0
  levels[r_i] <- levels[r_i] - 1
  tibble(value = values[-l_i], level = levels[-l_i])
}

test_explode %>% map(~ sfn_values(.x) %>% sfn_explode())

insert <- function(x, at, values) { # inserts after at
  prev <- x[1:at]
  post <- x[at:length(x)][-1]
  c(prev, values, post)
}

sfn_split <- function(sfn) {
  if (all(sfn$value < 10)) {
    return(sfn)
  } else {
    c(values, levels) %<-% sfn
  }
  c(s_i, ...rest = NULL) %<-% which(values >= 10)
  # Insert split values into the vectors
  half <- values[s_i] / 2
  tibble(
    value = insert(values, s_i, c(floor(half), ceiling(half)))[-s_i],
    level = insert(levels, s_i, levels[s_i] + c(1, 1))[-s_i]
  )
}

sfn_parse("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]")[[1]] %>% sfn_values() %>% sfn_explode() %>% sfn_split()

repeat_break <- function(x, f) {
  old <- x
  repeat {
    new <- f(old)
    if (identical(new, old)) {
      break
    }
    old <- new
  }
  new
}

sfn_reduce <- function(sfn) {
  reduce_cycle <- function(sfn) {
    sfn <- repeat_break(sfn, sfn_explode)
    sfn_split(sfn)
  }
  repeat_break(sfn, reduce_cycle)
}

sfn_parse("[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]")[[1]] %>% sfn_values() %>% sfn_reduce()

sfn_sum <- function(l, clear = FALSE) {
  l_values <- map(l, sfn_values)

  pb <- progress_bar$new(
    format = "  calculating [:bar] :current/:total (:percent) in :elapsed, eta :eta",
    total = length(l_values),
    clear = clear
  )
  pb$tick(0)

  sfn_add <- function(x, y) {
    bind_rows(x, y) %>%
      mutate(level = level + 1) %>%
      sfn_reduce()
  }

  reduce(l_values, ~ {pb$tick(); sfn_add(.x, .y)})
}

map(test_sums, sfn_sum)
sfn_sum(test_sums[[4]])



sfn_magnitude <- function(sfn) {
  if (nrow(sfn) > 1) {
    c(values, levels) %<-% sfn
    c(l_i, r_i, ...rest = NULL) %<-% which(levels == max(levels))
    m <- 3 * values[l_i] + 2 * values[r_i]
    values[r_i] <- m
    levels[r_i] <- levels[r_i] - 1
    sfn <- sfn_magnitude(tibble(value = values[-l_i], level = levels[-l_i]))
  } else {
    sfn <- sfn$value
  }
  sfn
}

test_magnitudes %>% map_dbl(~ sfn_values(.x) %>% sfn_magnitude())

q18a <- function(homework) {
  total <- sfn_sum(homework)
  sfn_magnitude(total)
}

q18a(test_18)
q18a(input_18)

q18b <- function(homework) {
  combos <- combn(homework, 2, simplify = FALSE)
  reversed <- map(combos, ~list(.x[[2]], .x[[1]]))
  combos <- append(combos, reversed)
  pb <- progress_bar$new(
    format = "  calculating [:bar] :current/:total (:percent) in :elapsed, eta :eta",
    total = length(combos),
    clear = FALSE
  )
  pb$tick(0)
  magnitudes <- map_dbl(combos, ~{pb$tick(); q18a(.x)})
  max(magnitudes)
}
q18b(test_18)
q18b(input_18)
