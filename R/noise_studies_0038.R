# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Source custom function -------------------------------------------------------

source(here::here("R/functions/weave_noise.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "noise_studies_0038"
seed_num <- 4878238
initial_grid_size <- 40
warp_factor <- 100 # lower = more warping
line_colour <- "#000000"
bg_colour <- "#FFFFFF"

colour_vec1 <- c("#006D77", "#42999B", "#83C5BE", "#EDF6F9")
colour_vec2 <- c("#A17E68", "#E29578", "#F1B9A5", "#FFDDD2")

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
    colour1 = sample(c(colour_vec1), n(), replace = TRUE),
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
