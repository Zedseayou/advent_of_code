library(tidyverse)

parse_input <- function(input) {
  input %>%
    enframe(name = NULL) %>%
    separate(value, c("input", "output"), sep = " \\| ") %>%
    mutate(across(.fns = ~ str_split(.x, " "))) # two column df with vectors of codes
}

test_8 <- read_lines("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce") %>% parse_input

input_8 <- read_lines("2021/data/input_8.txt") %>% parse_input

q8a <- function(input) {
  uniques <- input %>%
    pull(output) %>%
    flatten_chr() %>%
    str_length()

  sum(uniques %in% c(2, 3, 4, 7)) # just have to count lengths, no decoding
}

q8a(test_8)
q8a(input_8)



decode_segments <- function(input) {
  letters <- str_split(input, "")

  n <- lengths(letters)
  n
  one <- letters[[which(n == 2)]]
  four <- letters[[which(n == 4)]]
  seven <- letters[[which(n == 3)]]
  eight <- letters[[which(n == 7)]]
  n6 <- letters[n == 6]
  n5 <- letters[n == 5]

  three_index <- map_lgl(n5, ~ all(seven %in% .x))
  three <- n5[three_index][[1]] # of 2, 3, 5, only 3 has both the right segments
  n5 <- n5[-which(three_index)]

  six_index <- map_lgl(n6, ~ !all(one %in% .x))
  six <- n6[six_index][[1]] # of 0, 6, 9, only 6 doesn't have the right segments
  n6 <- n6[-which(six_index)]

  five_index <- map_lgl(n5, ~ all(.x %in% six))
  five <- n5[five_index][[1]] # of 2, 5, 5 is contained in 6
  two <- n5[!five_index][[1]]

  zero_index <- map_lgl(n6, ~ length(intersect(.x, four)) == 3)
  zero <- n6[zero_index][[1]] # zero shares 3 segements with 4, 9 shares 4
  nine <- n6[!zero_index][[1]]

  codes <- list(zero, one, two, three, four, five, six, seven, eight, nine) %>%
    map_chr(~ .x %>% sort() %>% str_c(collapse = ""))

  0:9 %>%
    as.character() %>%
    set_names(codes)
}

decode_segments(test_8$input[[1]])

calculate_output <- function(input, output) {
  codes <- decode_segments(input)

  out_indexes <- output %>%
    str_split("") %>%
    map_chr(~ .x %>% sort() %>% str_c(collapse = ""))

  codes[out_indexes] %>% # use the sorted codes as indexes to get the output
    str_c(collapse = "") %>%
    as.integer()
}

calculate_output(test_8$input[[1]], test_8$output[[1]])

q8b <- function(input) {
  sum(pmap_int(input, calculate_output))
}

q8b(test_8[2:11, ])
q8b(input_8)
