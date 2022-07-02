# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Source custom function -------------------------------------------------------

source(here::here("R/functions/weave_noise.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "noise_studies_0191"
seed_num <- 568191
initial_grid_size <- 40
warp_factor <- 20 # lower = more warping
colour_vec1 <- c("#F9345B", "#FF9D34", "#53FF45", "#009FB7", "#071422") # 5 cols
bg_colour <- "#071422"

# Make some noise --------------------------------------------------------------

# Generate data
grid <- weave_noise(
  seed = seed_num, x_max = 2, y_max = 3, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 3, geom_size_max = 9,
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
  geom_curve(
    data = grid,
    aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size * 65,
        colour = colour),
    curvature = -0.1) +
  geom_point(
    data = grid %>% filter(subset == 20),
    aes(x = x_warped, y = y_warped, size = size * 110, colour = colour),
    shape = 1) +
  #geom_point(
  #  data = grid %>% filter(subset < 12),
  #  aes(x = x_warped, y = y_warped, size = size * 1.5),
  #  colour = bg_colour) +
  geom_point(
    data = grid %>% filter(subset < 5 & subset > 2),
    aes(x = x_warped, y = y_warped, size = size / 2.5, colour = colour),
    shape = 6) +
  #geom_point(
  #  data = grid %>% filter(subset <= 2),
  #  aes(x = x_warped, y = y_warped, size = size / 2.5, colour = colour),
  #  shape = 5) +
  scale_size_identity() +
  scale_colour_identity() +
  coord_equal(expand = TRUE) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = bg_colour, colour = bg_colour),
    plot.margin     = margin(40,30,40,30, unit = "pt"))

# Export to file ---------------------------------------------------------------

ggsave(
  here::here(glue::glue("img/{`iteration_id`}.png")),
  device = "png", width = 4000, height = 6000, units = "px", dpi = 600)
