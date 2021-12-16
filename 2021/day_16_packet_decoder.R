library(tidyverse)

test_16a <- "D2FE28"
test_16b <- "38006F45291200"

input_16 <- read_file("2021/data/input_16.txt")

hex_map <- c("0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
names(hex_map) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F")



hex_table <- read_delim("0 = 0000
1 = 0001
2 = 0010
3 = 0011
4 = 0100
5 = 0101
6 = 0110
7 = 0111
8 = 1000
9 = 1001
A = 1010
B = 1011
C = 1100
D = 1101
E = 1110
F = 1111", delim = "=", col_names = c("hex", "bin"), trim_ws = TRUE)

to_binary <- function(hexstr) {
  hex_chars <- str_split(hexstr, "")[[1]]
  hex_map[hex_chars] %>%
    str_split("") %>%
    flatten_chr()
}
to_binary(test_16a)
to_binary(test_16b)

parse_packet <- function(bits) {

}


to_binary(test_16a)


as.hexmode(test_16a) %>% as.integer()

strtoi(test_16a, 16)

as.integer(intToBits(13827624))
