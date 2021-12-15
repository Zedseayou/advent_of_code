library(tidyverse)
library(tidygraph)

parse_input <- function(raw) {
  width <- read_lines(raw, n_max = 1) %>% str_length
  nodes <- raw %>%
    read_fwf(col_positions = fwf_widths(rep(1, width), col_names = 1:width)) %>%
    rowid_to_column(var = "row") %>%
    pivot_longer(-row, names_to = "col", values_to = "risk", names_transform = list(col = as.integer)) %>%
    rowid_to_column("node")
}

test_15 <- parse_input("1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581")

input_15 <- parse_input("2021/data/input_15.txt")

to_tidygraph <- function(nodes) {
  create_lattice(
    dim = c(max(nodes$row), max(nodes$col)),
    directed = TRUE,
    mutual = TRUE
  ) %>%
    activate(nodes) %>% # this appears to be the right ordering already
    mutate(node = row_number(), risk = nodes$risk) %>%
    activate(edges) %>%
    left_join(
      y = select(nodes, node, row_from = row, col_from = col),
      by = c("from" = "node")
    ) %>%
    left_join(
      y = rename(nodes, row_to = row, col_to = col),
      by = c("to" = "node")
    ) %>% # can't use .N() in left_join
    # mutate(risk = map_dbl(to, ~ .N()$risk[.N()$node[.x]])) %>%
    activate(nodes)
}

to_tidygraph(test_15)
to_tidygraph(input_15)

q15a <- function(input) {
  graph <- to_tidygraph(input)
  graph %>%
    convert(to_shortest_path, from = 1, to = nrow(input), weights = risk) %>%
    activate(edges) %>%
    pull(risk) %>%
    sum()
}
q15a(test_15)
q15a(input_15)

shift_grid_tile <- function(nodes, row_offset, col_offset) {
  n_rows <- max(nodes$row)
  n_cols <- max(nodes$col)

  nodes %>%
    mutate(
      nodes,
      row = row + (n_rows * row_offset),
      col = col + (n_cols * col_offset),
      risk = (risk + row_offset + col_offset) %% 9,
      risk = if_else(risk == 0, 9, risk) # 9 shouldn't wrap back to 0
    )
}

shift_grid_tile(test_15, 4, 4) %>% pull(col) %>% range()

tile_grid <- function(input) {
  map2_dfr(
    .x = rep(0:4, each = 5),
    .y = rep(0:4, times = 5),
    .f = ~ shift_grid_tile(input, .x, .y)
  ) %>%
    arrange(row, col) %>%
    mutate(node = row_number())
}
tile_grid(test_15)

q15b <- function(input) {
  nodes <- tile_grid(input)
  graph <- to_tidygraph(nodes)

  graph %>%
    convert(to_shortest_path, from = 1, to = nrow(nodes), weights = risk) %>%
    activate(edges) %>%
    pull(risk) %>%
    sum()
}

q15b(test_15)
q15b(input_15)

plot_sub_path <- function(input, grid = c("risk", "points", "tiles"), size = NULL) {
  path <- input %>%
    to_tidygraph() %>%
    convert(to_shortest_path, from = 1, to = nrow(input), weights = risk) %>%
    activate(edges) %>%
    data.frame()

  if (grid == "risk") {
    grid_geom <- geom_text(aes(x = col, y = row, label = risk), size = size)
  } else if (grid == "points") {
    grid_geom <- geom_point(aes(x = col, y = row), alpha = 0.2, size = size)
  } else if (grid == "tiles") {
    lines <- seq(0, max(input$col), length.out = 6) + 0.5
    grid_geom <- list(
      geom_vline(xintercept = lines, alpha = 0.2),
      geom_hline(yintercept = lines, alpha = 0.2)
    )
  }

  input %>%
    ggplot() +
    grid_geom +
    geom_segment(
      mapping = aes(x = col_from, y = row_from, xend = col_to, yend = row_to),
      data = path,
      alpha = 0.5,
      colour = "red"
    ) +
    # annotate("label", x = 1, y = 1, label = "start") +
    # annotate("label", x = max(input$col), y = max(input$row), label = "end") +
    scale_y_reverse() +
    theme_void()
}

plot_sub_path(test_15, grid = "risk", size = 6)
plot_sub_path(input_15, grid = "points", size = 0.1)

plot_sub_path(tile_grid(test_15), grid = "risk", size = 3)
plot_sub_path(tile_grid(test_15), grid = "tiles")
plot_sub_path(tile_grid(input_15), grid = "tiles")

x <- list(geom_vline(xintercept = 1:5), geom_hline(yintercept = 1:5))

ggplot(mtcars, aes(x = cyl, y = disp)) + x
