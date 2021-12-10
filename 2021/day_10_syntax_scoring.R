library(tidyverse)

input_10 <- read_lines("2021/data//input_10.txt")

test_10 <- read_lines("[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]") %>% str_split("")

scores <- tibble(char = c(")", "]", "}", ">"), score = c(3, 57, 1197, 25137))

cor <- str_split("{([(<{}[<>[]}>{[]{[(<()>", "")[[1]]
# at each close, sum between the opener and the close for the other brackets

check_line <- function(line) {
  brackets = tibble(
    char = c("(", ")", "[", "]", "{", "}", "<", ">"),
    is_close = rep(c(FALSE, TRUE), times = 4),
    b1 = c(1, -1, rep(0, 6)),
    b2 = c(0, 0, 1, -1, rep(0, 4)),
    b3 = c(rep(0, 4), 1, -1, 0, 0),
    b4 = c(rep(0, 6), 1, -1)
  )
  close_open <- c(")" = "(", "]" = "[", "}" = "{", ">" = "<")

  line_tbl <- line %>%
    enframe(name = NULL, value = "char") %>%
    left_join(brackets, by = "char")

  handle_chunk <- function(line_tbl) {
    chunk_end <- which.max(line_tbl$is_close)
    open_indexes <- which(line_tbl$char == close_open[line_tbl$char[chunk_end]])
    if (length(open_indexes) == 0) {
      return(line_tbl$char[chunk_end])
    }
    chunk_start <- max(open_indexes[open_indexes < chunk_end])
    # print(c(chunk_start, chunk_end))
    chunk <- slice(line_tbl, chunk_start:chunk_end)
    b_sums <- map_dbl(c("b1", "b2", "b3", "b4"), ~ sum(chunk[[.x]]))
    if(all(b_sums == 0)) {
      out <- slice(line_tbl, -(chunk_start:chunk_end))
    } else {
      out <- line_tbl$char[chunk_end]
    }
    out
  }
  working <- line_tbl
  while (inherits(working, "tbl")) {
    # Sys.sleep(0.5)
    working <- handle_chunk(working)
  }
  working
}
check_line(cor)
check_line(test_10[[3]])

map(test_10, check_line)


check_line <- function(line) {
  brackets = tibble(
    char = c("(", ")", "[", "]", "{", "}", "<", ">"),
    is_close = rep(c(FALSE, TRUE), times = 4),
    b1 = c(1, -1, rep(0, 6)),
    b2 = c(0, 0, 1, -1, rep(0, 4)),
    b3 = c(rep(0, 4), 1, -1, 0, 0),
    b4 = c(rep(0, 6), 1, -1)
  )
  close_open <- c(")" = "(", "]" = "[", "}" = "{", ">" = "<")

  line_tbl <- line %>%
    enframe(name = NULL, value = "char") %>%
    left_join(brackets, by = "char")


  is_legal <- function(char_index, line_tbl) {
    if (line_tbl$is_close[char_index]) {
      open_indexes <- which(line_tbl$char == close_open[line_tbl$char[char_index]])
      chunk_start <- max(open_indexes[open_indexes < char_index])

      b_sums <- map_dbl(
        .x = c("b1", "b2", "b3", "b4"),
        .f = ~ sum(line_tbl[[.x]][chunk_start:char_index])
      )
      # legal = all(b_sums == 0)
      legal = b_sums
    } else {
      legal = TRUE
    }
    legal
  }

  line_tbl %>%
    mutate(is_legal = map(1:nrow(line_tbl), is_legal, line_tbl = line_tbl))

}
check_line(cor) %>% view()
check_line(test_10[[1]]) %>% view()

q10a <- function(input) {
  pats <- c("\\(", "\\)", "\\[", "\\]", "\\{", "\\}", "<", ">")
  map(pats, ~ str_count(input, .x))
}

q10a(test_10)

map(c("(", ")", "[", "]", "{", "}", "<", ">"), ~ str_count(test_10, pattern = .x))
