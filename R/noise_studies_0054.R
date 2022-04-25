# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Source custom function -------------------------------------------------------

source(here::here("R/functions/weave_noise.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "noise_studies_0054"
seed_num <- 215754
initial_grid_size <- 40
warp_factor <- 100 # lower = more warping
line_colour <- "#000000"
bg_colour <- "#FFFFFF"

colour_vec1 <- c("#455E53", "#5B7255", "#718657", "#989D3E", "#E2D965") # 5 cols
colour_vec2 <- c("#EF7813", "#EE4A1C", "#ED1C24", "#883A57")
outline_colour <- "#235789"

# Make some noise --------------------------------------------------------------

# Generate data
grid <- weave_noise(
  seed = seed_num, x_max = 2, y_max = 3, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 2, geom_size_max = 6,
  noise_type = "simplex")

# Add colours
set.seed(seed_num)
grid <- grid %>%
  mutate(
    colour1 = case_when(
      y <= 0.25*max(grid$y)      ~ sample(
          colour_vec1, n(), replace = TRUE,
          prob = c(0.7, 0.15, 0.1, 0.025, 0.025)),
      y <= 0.5*max(grid$y) &
        y > 0.25*max(grid$y)     ~ sample(
          colour_vec1, n(), replace = TRUE,
          prob = c(0.5, 0.2, 0.15, 0.075, 0.075)),
      y <= 0.75*max(grid$y) &
        y > 0.5*max(grid$y)     ~ sample(
          colour_vec1, n(), replace = TRUE,
          prob = c(0.3, 0.25, 0.2, 0.125, 0.125)),
      y > 0.75*max(grid$y)      ~ sample(
          colour_vec1, n(), replace = TRUE,
          prob = c(0.1, 0.3, 0.25, 0.175, 0.175))),
    colour2 = sample(c(colour_vec2), n(), replace = TRUE))

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_curve(
    data = grid,
    aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size,
        colour = colour1),
    curvature = -0.2) +
  geom_point(
    data = grid %>% filter(subset >= 10),
    aes(x = x_warped, y = y_warped, size = size / 3),
    colour = bg_colour) +
  geom_point(
    data = grid %>% filter(subset < 10 & subset >= 6),
    aes(x = x_warped, y = y_warped, size = size / 2),
    colour = line_colour, shape = 0) +
  geom_point(
    data = grid %>% filter(subset < 6 & subset >= 3),
    aes(x = x_warped, y = y_warped, size = size / 2),
    colour = bg_colour, shape = 0) +
  geom_point(
    data = grid %>% filter(subset < 3),
    aes(x = x_warped, y = y_warped, size = size / 2, colour = colour2),
    shape = 15) +
  geom_path(
    data = grid %>% filter(subset >= 10),
    aes(x = x_warped, y = y_warped),
    colour = bg_colour, size = 0.2, linetype = "dotted") +
  scale_size_identity() +
  scale_colour_identity() +
  coord_equal(xlim = c(min(grid$x), max(grid$x)),
              ylim = c(min(grid$y), max(grid$y)), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin     = margin(40,30,40,30, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  device = "png", width = 4000, height = 6000, units = "px", dpi = 600)
