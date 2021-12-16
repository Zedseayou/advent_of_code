library(tidyverse)
library(usethis)

test_16a <- "D2FE28"
test_16b <- "38006F45291200"
test_16c <- "EE00D40C823060"
test_16d <- "8A004A801A8002F478"
test_16e <- "620080001611562C8802118E34"
test_16f <- "C0015000016115A2E0802F182340"
test_16g <- "A0016C880162017C3686B18A3D4780"

input_16 <- read_file("2021/data/input_16.txt") %>% str_trim()

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
hex_to_bin(test_16f) %>% length()
hex_to_bin(test_16g) %>% length()

bin_to_int <- function(bits) {
  str_c(bits, collapse = "") %>% strtoi(2)
}
bin_to_int(c("1", "0", "0"))

read_literal <- function(bits, debug = FALSE) { # only call on a packet I already know is literal
  stopifnot(length(bits) > 6)
  content <- bits[7:length(bits)]
  piece_starts <- seq(1, length(content), by = 5)
  # last value is 4 after the first 0 that starts a group
  content_end <- piece_starts[which.max(content[piece_starts] == "0")] + 4
  # drop the prefixes and return those as the literal value
  value <- bin_to_int(content[1:content_end][-piece_starts])
  # return the remaining bits as well
  bits <- content[-c(1:content_end)]
  if(debug) ui_info("Value: {value}")
  list(value = value, bits = bits)
}
read_literal(hex_to_bin(test_16a))

debug_bits <- function(bits, debug) {
  n_bits <- length(bits)
  if (debug) ui_todo("{n_bits} Bits: {ui_value(bits[1:min(c(n_bits, 55))])}{if_else(n_bits > 55, '...', '')}")
}

read_operator <- function(bits, length_type_id = c("0", "1"), debug = FALSE) {
  subpackets <- list()

  if (length_type_id == "0") {
    # next 15 bits represent length in bits of the sub-packets contained by this packet
    length <- bin_to_int(bits[8:22])
    # if (debug) ui_info("Length Type: {length_type_id} (Bits), Length: {length}")

    packet_bits <- bits[seq(23, length.out = length)] # Just the bits in this operator packet
    # debug_bits(packet_bits, debug)
    while (length(packet_bits) > 0) {
      subpacket <- read_packet(packet_bits, debug = debug)
      subpackets <- append(subpackets, list(subpacket))
      packet_bits <- subpacket[["bits"]]
      debug_bits(packet_bits, debug)
    }
    bits <- bits[(23 + length):length(bits)] # Need to pass on any other packets
    # debug_bits(bits, debug)

  } else if (length_type_id == "1") {
    # next 11 bits represent number of sub-packets contained by this packet
    length <- bin_to_int(bits[8:18])
    # if (debug) ui_info("Length Type: {length_type_id} (Packets), Length: {length}")

    bits <- bits[19:length(bits)]
    # debug_bits(bits, debug)
    while (length(subpackets) < length) {
      subpacket <- read_packet(bits, debug = debug)
      subpackets <- append(subpackets, list(subpacket))
      bits <- subpacket[["bits"]]
      # debug_bits(bits, debug)
    }
  }
  list(length = length, subpackets = subpackets, bits = bits)
}

read_packet <- function(bits, debug = FALSE) {
  packet_version <- bin_to_int(bits[1:3])
  packet_type <- bin_to_int(bits[4:6])
  debug_bits(bits, debug)
  # if (debug) {
  #   # ui_todo("Start Bits: (n={length(bits)}), {ui_value(bits)}")
  #   ui_done("Version: {packet_version}, Type: {packet_type} ({if_else(packet_type == 4, 'Literal', 'Operator')})")
  # }
  # Literal packets (version, type, value, remaining bits)

  if (packet_type == 4) {
    out <- append(
      list(version = packet_version, type = packet_type),
      read_literal(bits, debug = debug)
    )
  } else { # Operator packets (version, type, length_type, length, subpackets)
    length_type_id <- bits[7]
    out <- append(
      list(version = packet_version, type = packet_type, length_type = length_type_id),
      read_operator(bits, length_type_id, debug = debug)
    )
  }
  if (debug) {
    ui_done("Version: {packet_version}, Type: {packet_type} ({if_else(packet_type == 4, 'Literal', 'Operator')})")
    ui_info("Value: {out[['value']] %||% ''}, Length Type: {out[['length_type_id']] %||% ''} ({if_else(out[['length_type_id']] == '0', 'Bits', 'Packets')}), Length: {out[['length']] %||% ''}")
  }
  out
}
read_packet(hex_to_bin(test_16a), debug = TRUE) %>% str()
read_packet(hex_to_bin(test_16b), debug = TRUE) %>% str()
read_packet(hex_to_bin(test_16c), debug = TRUE) %>% str()
read_packet(hex_to_bin(test_16d), debug = TRUE) %>% str()
read_packet(hex_to_bin(test_16e), debug = TRUE) %>% str()
read_packet(hex_to_bin(test_16f), debug = TRUE) %>% str()
read_packet(hex_to_bin(test_16g), debug = TRUE) %>% str()
read_packet(hex_to_bin(input_16), debug = TRUE) %>% str()


q16a <- function(input) {
  packet_tree <- read_packet(hex_to_bin(input))
  depth <- vec_depth(packet_tree)
  flattener <- compose(!!!rep(list(flatten), times = depth))
  flattened <- flattener(packet_tree)
  flattened[names(flattened) == "version"] %>%
    as.integer() %>%
    sum()
}

q16a(test_16d)
q16a(test_16e)
q16a(test_16f)
q16a(test_16g)
q16a(input_16)

# not getting rid of this ridiculous debugging to find an off by one error
x <- test_16f %>% hex_to_bin()
names(x)[1:3] <- "V"   # version 6
names(x)[4:6] <- "T"   # type 0 - operator
names(x)[7] <- "LT"    # length type 0 (bits)
names(x)[8:22] <- "L"  # length 84
names(x)[23:25] <- "V"   # version 0
names(x)[26:28] <- "T"   # type 0 - operator
names(x)[29] <- "LT"     # length type 0 (bits)
names(x)[30:44] <- "L"   # length 22
names(x)[45:47] <- "V"     # version 0
names(x)[48:50] <- "T"     # type 4 - literal
names(x)[51:55] <- "A"     # starts with 0, so value is 10
names(x)[56:58] <- "V"     # version 6
names(x)[59:61] <- "T"     # type 4 - literal
names(x)[62:66] <- "A"     # starts with 0, so value is 11
names(x)[67:69] <- "V"   # version 4
names(x)[70:72] <- "T"   # type 0 - operator

y <- c('0', '1', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '1', '0', '0', '0', '0', '1', '1', '0', '1', '1', '0', '0', '0', '1', '0', '1', '0', '0', '1', '1', '1', '0', '0', '0', '0', '0', '1', '0', '1', '0', '0', '1', '0', '0', '0', '1', '0', '1', '0')

read_packet(y)
