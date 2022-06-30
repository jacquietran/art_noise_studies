# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Source custom function -------------------------------------------------------

source(here::here("R/functions/weave_noise.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "noise_studies_0163"
seed_num <- 578163
initial_grid_size <- 30
warp_factor <- 50 # lower = more warping
line_colour <- "#1F1F1F"
bg_colour <- "#E4DEDD"

# Make some noise --------------------------------------------------------------

# Generate data
grid <- weave_noise(
  seed = seed_num, x_max = 2, y_max = 3, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 3, geom_size_max = 9,
  noise_type = "value")

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_curve(
    data = grid,
    aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size),
    colour = line_colour, curvature = -0.1) +
  geom_point(
    data = grid %>% filter(subset >= 20),
    aes(x = x_warped, y = y_warped, size = size * 10),
    colour = bg_colour, shape = 1) +
  geom_point(
    data = grid %>% filter(subset < 10 & subset >= 8),
    aes(x = x_warped, y = y_warped, size = size / 2.5),
    colour = line_colour, shape = 0) +
  geom_point(
    data = grid %>% filter(subset < 8 & subset >= 5),
    aes(x = x_warped, y = y_warped, size = size * 1.25),
    colour = bg_colour, shape = 18) +
  scale_size_identity() +
  scale_colour_identity() +
  coord_equal(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin     = margin(40,30,40,30, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  device = "png", width = 4000, height = 6000, units = "px", dpi = 600)
