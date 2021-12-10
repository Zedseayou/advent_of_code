library(tidyverse)

input_10 <- read_lines("2021/data//input_10.txt") %>% str_split("")

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

check_line <- function(line, debug = FALSE) {
  open <- c("(", "[", "{", "<")
  close <- c(")", "]", "}", ">")
  names(close) <- open
  expected <- character()

  for (c in line) {
    if (debug) {
      Sys.sleep(0.5)
      msg <- str_glue("Expecting {expected[1]}, found {c}")
      message(msg)
    }

    if (c %in% open) {
      expected <- c(unname(close[c]), expected)
    } else if (c %in% close) {
      if (c == expected[1]) {
        expected <- expected[-1]
      } else if (c != expected[1]) {
        return(c)
      }
    }
  }
  return("not corrupted")
}
check_line(test_10[[1]], debug = TRUE)
check_line(test_10[[3]], debug = TRUE)


q10a <- function(input, debug = FALSE) {
  scores <- tibble(char = c(")", "]", "}", ">"), score = c(3, 57, 1197, 25137))
  line_chars <- map_chr(input, check_line, debug = debug)

  line_chars %>%
    enframe(name = NULL, value = "char") %>%
    filter(char != "not corrupted") %>%
    left_join(scores, by = "char") %>%
    pull(score) %>%
    sum()
}

q10a(test_10)
q10a(input_10, debug = FALSE)

complete_line <- function(line, debug = FALSE) {
  open <- c("(", "[", "{", "<")
  close <- c(")", "]", "}", ">")
  names(close) <- open
  expected <- character()

  for (c in line) {
    if (debug) {
      Sys.sleep(0.5)
      msg <- str_glue("Stack looks like {str_c(expected, collapse = ', ')}")
      message(msg)
    }

    if (c %in% open) {
      expected <- c(unname(close[c]), expected)
    } else if (c == expected[1]) {
      expected <- expected[-1]
    }
  }
  expected
}

complete_line(test_10[[1]], debug = FALSE)

q10b <- function(input, debug = FALSE) {
  is_corrupt <- map_chr(input, check_line, debug = debug)
  valid_lines <- input[is_corrupt == "not corrupted"]

  scores <- tibble(char = c(")", "]", "}", ">"), score = 1:4)

  completions <- map(valid_lines, complete_line)

  completions %>%
    enframe(name = NULL, value = "chars") %>%
    rowid_to_column(var = "line_id") %>%
    unnest(chars) %>%
    left_join

}
