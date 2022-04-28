# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Source custom function -------------------------------------------------------

source(here::here("R/functions/weave_noise.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "noise_studies_0130"
seed_num <- 1587130
initial_grid_size <- 150
warp_factor <- 70 # lower = more warping
colour_vec1 <- c("#EFDC9E", "#F5CB5C", "#F4C23B", "#F38F1D", "#F2A81D") # 5 cols
bg_colour <- "#E8EDDF"

# Make some noise --------------------------------------------------------------

# Generate data
grid <- weave_noise(
  seed = seed_num, x_max = 1, y_max = 1.5, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 0.5, geom_size_max = 3,
  noise_type = "cubic")

# Add colours
set.seed(seed_num)
grid <- grid %>%
  mutate(
    colour = case_when(
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
        prob = c(0.1, 0.3, 0.25, 0.175, 0.175))))

# Build plot -------------------------------------------------------------------

ggplot() +
  ggfx::with_shadow(
    geom_curve(
      data = grid,
      aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size,
          colour = colour),
      curvature = -0.2),
    sigma = 1, colour = "#885B07") +
  geom_point(
    data = grid %>% filter(subset >= 10),
    aes(x = x_warped, y = y_warped, size = size / 3),
    colour = bg_colour) +
  geom_point(
    data = grid %>% filter(subset < 10 & subset >= 3),
    aes(x = x_warped, y = y_warped, size = size / 2, colour = colour),
    shape = 0) +
  geom_point(
    data = grid %>% filter(subset < 3),
    aes(x = x_warped, y = y_warped, size = size / 2, colour = colour),
    shape = 15) +
  scale_size_identity() +
  scale_colour_identity() +
  coord_equal(expand = FALSE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin     = margin(80,60,80,60, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  device = "png", width = 4000, height = 6000, units = "px", dpi = 600)
