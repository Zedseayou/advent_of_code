library(tidyverse)
library(zeallot)

SnailfishNumber <- R6::R6Class(
  classname = "SnailfishNumber",

  public = list(
    left = NULL,
    right = NULL,
    depth = NULL,
    initialize = function(sfn_list, depth = 0) { # 0 depth means not nested
      stopifnot(length(sfn_list) == 2)

      self$left <- sfn_list[[1]]
      self$right <- sfn_list[[2]]
      self$depth <- depth

      self$left <- private$atomic_if_else(
        child = "left",
        true = self$left,
        false = SnailfishNumber$new(self$left, depth + 1)
      )
      self$right <- private$atomic_if_else(
        child = "right",
        true = self$right,
        false = SnailfishNumber$new(self$right, depth + 1)
      )
    },
    to_list = function(...) {
      left <- private$atomic_if_else("left", self$left, self$left$to_list())
      right <- private$atomic_if_else("right", self$right, self$right$to_list())
      pair_names <- str_c(c("L", "R"), self$depth + 1)
      list(left, right) %>% `names<-`(pair_names)
    },
    print = function(...) {
      str(self$to_list())
    },
    explode = function(...){
      if ()
    }
  ),

  private = list(
    atomic_if_else = function(child = c("left", "right"), true, false) {
      if (child == "left") {
        if (is_atomic(self$left)) {
          return(true)
        } else {
          return(false)
        }
      } else if (child == "right") {
        if (is_atomic(self$right)) {
          return(true)
        } else {
          return(false)
        }
      }
    }
  )
)

SnailfishNumber$new(list(list(1, 2), 3))$to_list() %>% str()
SnailfishNumber$new(test_18[[7]])

sfn_parse <- function(raw) {
  raw %>%
    read_lines() %>%
    str_replace_all("\\]", "\\)") %>%
    str_replace_all("\\[", "list\\(") %>%
    map(~SnailfishNumber$new(eval(parse(text = .x))))
}
sfn_parse("[[[[[9,8],1],2],3],4]\n")

test_18 <- sfn_parse("[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")

input_18 <- sfn_parse("2021/data/input_18.txt")

sfn_explode <- function(line) {

}

sfn_parse <- function(raw) {

}

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
  c(lpos, rpos, ...rest = NULL) %<-% which(values > 4)
  if (lpos > 1) values[lpos - 1] <- values[lops - 1] + values[lpos]
  if (rpos < length(values)) values[rpos + 1] <- values[rpos + 1] + values[rpos]
  values[rpos] <- 0
  levels[rpos] <- levels[rpos] - 1
  tibble(value = values[-lpos], level = levels[-lpos])
}

sfn_values(x) %>% sfn_explode()

bench::mark(
  tibble(x = letters),
  list(x = letters),
  check = FALSE
)


x <- "[[[[[9,8],1],2],3],4]" %>%
  str_split("") %>%
  pluck(1)
  enframe(name = NULL, value = "char") %>%
  filter(char != ",") %>%
  mutate(
    level = cumsum((char == "[") - (char == "]"))
  ) %>%
  filter(str_detect(char, "[0-9]"))



sfn_add <- function(x, y) {
  list(x, y)
}

sfn_reduce <- function(x) {

}

sfn_parse("[[[[[9,8],1],2],3],4]\n") %>% str()

vec_depth(list(1, 2))

#   printstr = function(...) {
#     # indent <- str_c(rep(" ", (4 - self$depth) * 2), collapse = "")
#     c(private$leftstr(), private$rightstr())
#   },
#   print = function(...) {
#     cat(self$printstr(), sep = "\n")
#   }
# ),

# private = list(
#   leftstr = function(...) {
#     indent <- str_c(rep(" ", (4 - self$depth) * 2), collapse = "")
#     if (is_atomic(self$left)) {
#       leftstr <- str_c(indent, "L:", self$left)
#     } else {
#       leftstr <- self$left$numstr()
#     }
#     leftstr
#   },
#   rightstr = function(...) {
#     indent <- str_c(rep(" ", (4 - self$depth) * 2), collapse = "")
#     if (is_atomic(self$right)) {
#       rightstr <- str_c(indent, "R:", self$right)
#     } else {
#       rightstr <- self$right$numstr()
#     }
#     rightstr
#   }
# )
