library(tidyverse)
library(progress)

parse_input <- function(raw) {
  tibble(raw = raw) %>%
    extract(
      col = raw,
      into = c("xmin", "xmax", "ymin", "ymax"),
      regex = "x=([-|\\d]+)..([-|\\d]+), y=([-|\\d]+)..([-|\\d]+)",
      convert = TRUE
    )
}

test_17 <- parse_input("target area: x=20..30, y=-10..-5")
input_17 <- parse_input("target area: x=257..286, y=-101..-57")

vx <- function(t, vx_0) {
  increment <- if_else(vx_0 >= 0, -1, 1)
  if (abs(vx_0) >= t) { # need to truncate the sequence
    out <- seq(vx_0, by = increment, length.out = t)
  } else { # need to pad the sequence
    to_v0 <- seq(vx_0, to = 0, by = increment)
    pad <- t - length(to_v0)
    out <- c(to_v0, rep(0, length.out = pad))
  }
  out
}
vx(7, 10)
vx(10, 10)
vx(15, -10)

vy <- function(t, vy_0) seq(vy_0, by = -1, length.out = t)
vy(5, 10)

trajectory <- function(t, vx_0, vy_0) {
  tibble(vx = vx(t, vx_0), vy = vy(t, vy_0), x = cumsum(vx), y = cumsum(vy))
}
trajectory(50, 7, 2)
trajectory(25, 6, 9) %>% view()
trajectory(25, 6, 10) %>% view()
trajectory(10, 2, -1)

plot_trajectory <- function(t, vx_0, vy_0, target = NULL, zoom = FALSE) {
  s <- trajectory(t, vx_0, vy_0)
  p <- s %>%
    bind_rows(tibble(x = 0, y = 0)) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(colour = "red") +
    geom_line(colour = "red", alpha = 0.2) +
    theme_bw()

  if (!is.null(target)) {
    p <- p + geom_rect(
      data = target,
      mapping = aes(x = NULL, y = NULL, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = NA,
      colour = "black"
    )
    if (zoom) {
      p <- p + coord_cartesian(xlim = c(target$xmin, target$xmax), ylim = c(target$ymin, target$ymax))
    }
  }
  p
}
plot_trajectory(10, 7, 2, test_17)
plot_trajectory(20, 6, 9, test_17)
plot_trajectory(20, 7, 9, test_17)
plot_trajectory(25, 6, 10, test_17)

# pick vx_0 so the ball just reaches the target
min_vx_0 <- function(target) {
  x_vals <- cumsum(1:target$xmin) # overkill
  which(x_vals >= target$xmin & x_vals <= target$xmax)[1]
}
min_vx_0(test_17)
min_vx_0(input_17)

q17a <- function(target) {
  vx_val <- min_vx_0(target)[1]
  # for negative target y, the ball will always pass through 0 with vy = -vy_0
  # biggest vy_0 therefore is when it takes only 1 step from y = 0 to the target
  vy_val <- abs(target$ymin) -1
  max_y <- trajectory(2 * vy_val + 10, vx_val, vy_val) %>%
    pull(y) %>%
    max()
  c(vx_0 = vx_val, vy_0 = vy_val, max_y = max_y)
}
q17a(test_17)
q17a(input_17)

on_target <- function(t, vx_0, vy_0, target) {
  s <- trajectory(t, vx_0, vy_0)
  hits <- s$x >= target$xmin & s$x <= target$xmax & s$y >= target$ymin & s$y <= target$ymax
  if (any(hits)) {
    out <- which.max(hits)
  } else {
    out <- 0L
  }
  out
}

on_target(50, 7, 2, test_17)
on_target(50, 6, 3, test_17)
on_target(50, 9, 0, test_17)
on_target(50, 17, -4, test_17)
on_target(50, 6, 9, test_17)
on_target(250, 23, 100, input_17)

q17b <- function(target) {
  # Bounds
  # vx_0 must be large enough to reach x by the time speed is 0
  # vx_0 must be smaller than xmax, else it will pass the target in one step
  # vy_0 must be larger than ymin, else it will pass through the target in one step
  # vy_0 must be smaller than the abs(ymin) bound from part 1
  # t should be y-limited; probably not more than 2.5x vy_0
  search_space <- crossing(
    vx_0 = min_vx_0(target):target$xmax,
    vy_0 = target$ymin:abs(target$ymin)
  ) %>%
    transmute(t = as.integer(ceiling(2.5 * max(vy_0))), vx_0, vy_0)
  pb <- progress_bar$new(
    format = "  calculating [:bar] :current/:total (:percent) in :elapsed, eta :eta",
    total = nrow(search_space),
    clear = FALSE
  )
  pb$tick(0)
  # search_space
  t_hit <- pmap_int(search_space, ~ {pb$tick(); on_target(..1, ..2, ..3, target)})
  search_space %>%
    mutate(t_hit = t_hit) %>%
    filter(t_hit > 0L)
}
q17b(test_17) -> test_hits
q17b(input_17) -> input_hits

input_hits %>% nrow

plot_trajectory(1, 270, -80, input_17)
plot_trajectory(83, 23, 40, input_17)
plot_trajectory(19, 24, 6, input_17)

input_hits %>% filter(vx_0 > 23, vy_0 > 0)
