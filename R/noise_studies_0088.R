# Load libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ambient)

# Source custom function -------------------------------------------------------

source(here::here("R/functions/weave_noise.R"))

# Modifiable parameters --------------------------------------------------------

iteration_id <- "noise_studies_0088"
seed_num <- 215788
initial_grid_size <- 15
warp_factor <- 30 # lower = more warping
line_colour <- "#FEA802"
bg_colour <- "#EEECE7"

colour_vec1 <- c("#B4418E", "#D94A8C", "#EA515F", "#FE7434", "#FEA802") # 5 cols
colour_vec2 <- c("#0450B4", "#046DC8", "#1184A7", "#15A2A2", "#6FB1A0")
outline_colour <- "#0C3264"

# Make some noise --------------------------------------------------------------

# Generate data
grid <- weave_noise(
  seed = seed_num, x_max = 1, y_max = 1.5, grid_length = initial_grid_size,
  warp_factor = warp_factor, geom_size_min = 3, geom_size_max = 8,
  noise_type = "value")

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
  ggfx::as_reference(
    geom_curve(
      data = grid,
      aes(x = x, y = y, xend = x_warped, yend = y_warped, size = size,
          colour = colour1),
      curvature = -0.2),
    id = "base") +
  geom_point(
    data = grid %>% filter(subset >= 10),
    aes(x = x_warped, y = y_warped, size = size / 3),
    colour = bg_colour) +
  ggfx::with_blend(
    geom_point(
      data = grid %>% filter(subset < 8 & subset >= 4),
      aes(x = x_warped, y = y_warped, size = size * 1.2, colour = colour2),
      shape = 16),
    bg_layer = "base",
    blend_type = "overlay") +
  ggfx::with_blend(
    geom_point(
      data = grid %>% filter(subset < 4),
      aes(x = x_warped, y = y_warped, size = size * 1.2, colour = colour2),
      shape = 16),
    bg_layer = "base",
    blend_type = "hard_light") +
  geom_point(
    data = grid %>% filter(subset < 10 & subset >= 6),
    aes(x = x_warped, y = y_warped, size = size * 0.8),
    colour = outline_colour, shape = 5) +
  geom_path(
    data = grid %>% filter(subset <= 10),
    aes(x = x_warped, y = y_warped),
    colour = bg_colour, size = 0.4, linetype = "dotted") +
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
