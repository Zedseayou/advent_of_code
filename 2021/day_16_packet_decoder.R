library(tidyverse)

test_16a <- "D2FE28"
test_16b <- "38006F45291200"

input_16 <- read_file("2021/data/input_16.txt")

hex_map <- c("0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111", "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111")
names(hex_map) <- c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F")


hex_to_bin <- function(hexstr) {
  hex_chars <- str_split(hexstr, "")[[1]]
  hex_map[hex_chars] %>%
    str_split("") %>%
    flatten_chr()
}
hex_to_bin(test_16a)
hex_to_bin(test_16b)

bin_to_int <- function(bits) {
  str_c(bits, collapse = "") %>% strtoi(2)
}
bin_to_int(c("1", "0", "0"))

read_literal <- function(bits) { # only call on a packet I already know is literal
  content <- bits[7:length(bits)]
  n_pieces <- ceiling(length(content) / 5)
  pieces <- split(content, rep(1:n_pieces, each = 5, length.out = length(content)))
  last_piece <- which.max(map_chr(pieces, 1) == "0")
  value <- pieces[1:last_piece] %>%
    map(~ .x[-1]) %>%
    flatten_chr() %>%
    bin_to_int()
  list()
}
read_literal(hex_to_bin(test_16a))

read_operator_0 <- function(bits) {
  bit_length <- bin_to_int(bits[8:22]) # 15 bits
  content <- bits[seq(23, length.out = bit_length)]
  read_literal(content)
}

read_operator_0(hex_to_bin(test_16b))


read_packet <- function(bits) {
  packet_version <- bin_to_int(bits[1:3])
  packet_type <- bin_to_int(bits[4:6])

  if(packet_type == 4) {
    out <- list(
      version = packet_version,
      type = packet_type,
      content = read_literal(bits)
    )
    return(out)
  } else {
    length_type_id <- bits[7]
    if (length_type_id == "0") {
       # 15 bits
    } else if (length_type_id == "1") {
      contained_n_packets <- bits[8:18] # 11 bits
    }
  }
}
read_packet(hex_to_bin(test_16a))


read_stream <- function(bits) {
  packet_structure <- list()
  read <- character()
  for (b in bits) {
    read <-c(read, b)

    if (length(read) == 6) {
      packet_version <- bin_to_int(read[1:3])
      packet_type <- bin_to_int(read[1:6])

      if (packet_type == 4) {

      }

    }


  }
}
