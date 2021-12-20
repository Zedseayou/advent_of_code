library(tidyverse)
library(zeallot)
library(progress)

parse_input <- function(raw) {
  lines <- read_lines(raw) %>%
    str_replace_all("\\.", "0") %>%
    str_replace_all("#", "1")

  image <- lines[3:length(lines)] %>%
    str_split("") %>%
    enframe(name = "row") %>%
    mutate(col = map(value, ~1:length(.x))) %>%
    unnest(c(col, value))

  algorithm <- lines[1] %>%
    str_split("") %>%
    pluck(1) %>%
    enframe(name = "index") %>%
    mutate(index = index - 1)

  list(image = image, algorithm = algorithm)
}

c(test_20_img, test_20_algo) %<-% parse_input("2021/data/test_20.txt")
c(input_20_img, input_20_algo) %<-% parse_input("2021/data/input_20.txt")
c(reddit_20_img, reddit_20_algo) %<-% parse_input("2021/data/test_20_reddit.txt")

plot_image <- function(image) {
  image %>%
    ggplot(aes(x = col, y = row)) +
    geom_raster(aes(fill = value)) +
    scale_fill_manual(values = c("#636363", "#f0f0f0")) +
    scale_y_reverse() +
    coord_equal() +
    theme_void() +
    theme(legend.position = "none") +
    labs(subtitle = str_glue("{max(image$row)}x{max(image$col)} image"))
}

plot_image(test_20_img)

image_enhance <- function(image, algorithm, pad) {
  adj <- crossing(x = c(1L, 0L, -1L), y = c(1L, 0L, -1L))
  expanded <- complete(
    data = image,
    row = 0:(max(row) + 1),
    col = 0:(max(col) + 1),
    fill = list(value = pad)
  )

  expanded %>%
    complete() %>%
    crossing(adj) %>%
    mutate(row2 = row + x, col2 = col + y) %>%
    left_join(expanded, by = c("row2" = "row", "col2" = "col"), suffix = c("", "2")) %>%
    mutate(value2 = coalesce(value2, pad)) %>%  # Values around the outside are same as the pad value
    group_by(row, col) %>%
    summarise(binary = str_c(value2, collapse = "") %>% strtoi(2), .groups = "drop") %>%
    left_join(algorithm, by = c("binary" = "index")) %>%
    transmute(row = row + 1, col = col + 1, value)
}

image_enhance(test_20_img, test_20_algo, "0") %>% view()

plot_image(test_20_img %>% image_enhance(test_20_algo, "0"))
plot_image(test_20_img %>% image_enhance(test_20_algo, "0") %>% image_enhance(test_20_algo, "0"))

q20a <- function(image, algorithm, n) {
  if (algorithm$value[1] == "0") { # outer dark will always stay dark
    image <- reduce(1:n, ~ image_enhance(.x, algorithm, pad = "0"), .init = image)
  } else {
    if (algorithm$value[512] == "0") { # outer areas will light after odd cycles
      pads <- if_else(1:n %% 2 == 1, "0", "1")
      image <- reduce(pads,  ~ image_enhance(.x, algorithm, pad = .y), .init = image)
    }
    # other cases not implemented :D
  }
  sum(image$value == "1")
}

q20a(test_20_img, test_20_algo, 2)
q20a(input_20_img, input_20_algo, 2)
q20a(reddit_20_img, reddit_20_algo, 2)

q20a(test_20_img, test_20_algo, 50)
q20a(input_20_img, input_20_algo, 50)

