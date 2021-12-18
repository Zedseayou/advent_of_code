library(tidyverse)

SnailfishNumber <- R6::R6Class(
  classname = "SnailfishNumber",
  public = list(
    left = NULL,
    right = NULL,
    depth = NULL,
    initialize = function(sfn_list) {
      stopifnot(length(sfn_list) == 2)

      left <- sfn_list[[1]]
      right <- sfn_list[[2]]
      self$depth <- 0 # Number of contained SnailfishNumbers
      if (is_atomic(left)) {
        self$left <- left
      } else {
        self$depth <- self$depth + 1
        self$left <- SnailfishNumber$new(left)
      }

      if (is_atomic(right)) {
        self$right <- right
      } else {
        self$depth <- self$depth + 1
        self$right <- SnailfishNumber$new(right)
      }
    },
    printstr = function(...) {
      # indent <- str_c(rep(" ", (4 - self$depth) * 2), collapse = "")
      c(private$leftstr(), private$rightstr())
    },
    print = function(...) {
      cat(self$printstr(), sep = "\n")
    }
  ),

  private = list(
    leftstr = function(...) {
      indent <- str_c(rep(" ", (4 - self$depth) * 2), collapse = "")
      if (is_atomic(self$left)) {
        leftstr <- str_c(indent, "L:", self$left)
      } else {
        leftstr <- self$left$numstr()
      }
      leftstr
    },
    rightstr = function(...) {
      indent <- str_c(rep(" ", (4 - self$depth) * 2), collapse = "")
      if (is_atomic(self$right)) {
        rightstr <- str_c(indent, "R:", self$right)
      } else {
        rightstr <- self$right$numstr()
      }
      rightstr
    }
  )
)
SnailfishNumber$new(list(1, 2))

SnailfishNumber$new(list(list(1, 2), 3))
SnailfishNumber$new(test_18[[7]])

sfn_parse <- function(raw) {
  raw %>%
    read_lines() %>%
    str_replace_all("\\]", "\\)") %>%
    str_replace_all("\\[", "list\\(") %>%
    map(~SnailfishNumber$new(eval(parse(text = .x))))
}

test_18 <- sfn_parse("[1,2]
[[1,2],3]
[9,[8,7]]
[[1,9],[8,5]]
[[[[1,2],[3,4]],[[5,6],[7,8]]],9]
[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]
[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]")

input_18 <- sfn_parse("2021/data/input_18.txt")

sfn_add <- function(x, y) {
  list(x, y)
}

sfn_reduce <- function(x) {

}

sfn_parse("[[[[[9,8],1],2],3],4]\n") %>% str()

vec_depth(list(1, 2))
