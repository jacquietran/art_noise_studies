# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Source custom function -------------------------------------------------------

source(here::here("R/functions/weave_noise.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "noise_studies_0129"
seed_num <- 5687129
initial_grid_size <- 6
warp_factor <- 20 # lower = more warping
line_colour1 <- "#6DA78B"
line_colour2 <- "#F5CB5C"
bg_colour <- "#000000"
path_colour <- "#E8EDDF"

# Make some noise --------------------------------------------------------------

set.seed(seed_num)
seed_vec <- sample(seq(1, 1000000, by = 1), 2, replace = FALSE)

# Generate data
grid1 <- weave_noise(
  seed = seed_vec[1], x_max = 2, y_max = 3, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 6, geom_size_max = 18,
  noise_type = "simplex")

grid2 <- weave_noise(
  seed = seed_vec[2], x_max = 2, y_max = 3, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 6, geom_size_max = 18,
  noise_type = "simplex")

# Build plot -------------------------------------------------------------------

ggplot() +
  # Grid 1
  geom_curve(
    data = grid1,
    aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size),
    colour = line_colour1, curvature = -0.4) +
  # Grid 2
  geom_curve(
    data = grid2,
    aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size),
    colour = line_colour2, curvature = -0.4) +
  geom_point(
    data = grid2 %>% filter(subset >= 10),
    aes(x = x_warped, y = y_warped, size = size * 0.45),
    colour = bg_colour) +
  geom_point(
    data = grid2 %>% filter(subset < 13 & subset >= 3),
    aes(x = x_warped, y = y_warped, size = size / 2.5),
    colour = path_colour, shape = "|") +
  geom_point(
    data = grid2 %>% filter(subset < 5),
    aes(x = x_warped, y = y_warped, size = size * 0.45),
    colour = path_colour, shape = 18) +
  geom_curve(
    data = grid %>% filter(subset >= 12),
    aes(x = x, y = y, xend = x_warped, yend = y_warped),
    colour = path_colour, size = 0.3, curvature = -0.2, linetype = "dashed") +
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
