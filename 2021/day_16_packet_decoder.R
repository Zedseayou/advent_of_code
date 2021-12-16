library(tidyverse)
library(usethis)

test_16a <- "D2FE28"
test_16b <- "38006F45291200"
test_16c <- "EE00D40C823060"

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
hex_to_bin(test_16c)

bin_to_int <- function(bits) {
  str_c(bits, collapse = "") %>% strtoi(2)
}
bin_to_int(c("1", "0", "0"))

read_literal <- function(bits) { # only call on a packet I already know is literal
  stopifnot(length(bits) > 6)
  content <- bits[7:length(bits)]
  piece_starts <- seq(1, length(content), by = 5)
  # last value is 4 after the first 0 that starts a group
  content_end <- piece_starts[which.max(content[piece_starts] == "0")] + 4
  # drop the prefixes and return those as the literal value
  value <- bin_to_int(content[1:content_end][-piece_starts])
  # return the remaining bits as well
  bits <- content[-c(1:content_end)]
  list(value = value, bits = bits)
}
read_literal(hex_to_bin(test_16a))

read_operator <- function(bits, length_type_id = c("0", "1"), debug = FALSE) {
  subpackets <- list()

  if (length_type_id == "0") {
    # next 15 bits represent length in bits of the sub-packets contained by this packet
    length <- bin_to_int(bits[8:22])
    packet_bits <- bits[seq(23, length.out = length)]
    if (debug) ui_info("Bits: {ui_value(packet_bits)}")
    while (length(packet_bits) > 0) {
      subpacket <- read_packet(packet_bits)
      subpackets <- append(subpackets, list(subpacket))
      packet_bits <- subpacket[["bits"]]
      if (debug && length(packet_bits) > 0) ui_info("Bits: {ui_value(packet_bits)}")
    }

  } else if (length_type_id == "1") {
    # next 11 bits represent number of sub-packets contained by this packet
    length <- bin_to_int(bits[8:18])
    packet_bits <- bits[19:length(bits)]
    if (debug) ui_info("Bits: {ui_value(packet_bits)}")
    while (length(subpackets) < length) {
      subpacket <- read_packet(packet_bits)
      subpackets <- append(subpackets, list(subpacket))
      packet_bits <- subpacket[["bits"]]
      if (debug && length(packet_bits) > 0) ui_info("Bits: {ui_value(packet_bits)}")
    }
  }
  list(length = length, subpackets = subpackets)
}

read_packet <- function(bits, debug = FALSE) {
  packet_version <- bin_to_int(bits[1:3])
  packet_type <- bin_to_int(bits[4:6])

  if (debug) ui_info("Version: {packet_version}, Type: {packet_type}")

  # Literal packets (version, type, value, remaining bits)
  if (packet_type == 4) {
    out <- append(
      list(version = packet_version, type = packet_type),
      read_literal(bits)
    )
  } else { # Operator packets (version, type, length_type, length, subpackets)
    length_type_id <- bits[7]
    out <- append(
      list(version = packet_version, type = packet_type, length_type = length_type_id),
      read_operator(bits, length_type_id, debug = debug)
    )
  }
  out
}
read_packet(hex_to_bin(test_16a))
read_packet(hex_to_bin(test_16b), debug = TRUE)
read_packet(hex_to_bin(test_16c), debug = TRUE)

