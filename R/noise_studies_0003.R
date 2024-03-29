# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Source custom function -------------------------------------------------------

source(here::here("R/functions/weave_noise.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "noise_studies_0003"
seed_num <- 4878203
initial_grid_size <- 50
warp_factor <- 10 # lower = more warping
line_colour <- "#000000"
bg_colour <- "#FFFFFF"

# Make some noise --------------------------------------------------------------

# Generate data
grid <- weave_noise(
  seed = seed_num, x_max = 2, y_max = 3, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 0.5, geom_size_max = 3,
  noise_type = "cubic")

# Build plot -------------------------------------------------------------------

ggplot() +
  geom_curve(
    data = grid,
    aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size),
    colour = line_colour, curvature = -0.2) +
  geom_point(
    data = grid %>% filter(subset >= 10),
    aes(x = x_warped, y = y_warped, size = size / 3),
    colour = bg_colour) +
  geom_point(
    data = grid %>% filter(subset < 10 & subset >= 3),
    aes(x = x_warped, y = y_warped, size = size / 2),
    colour = line_colour, shape = 0) +
  geom_point(
    data = grid %>% filter(subset < 3),
    aes(x = x_warped, y = y_warped, size = size / 2),
    colour = line_colour, shape = 15) +
  geom_path(
    data = grid,
    aes(x = x_warped, y = y_warped),
    colour = bg_colour, size = 0.2, linetype = "dotted") +
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
