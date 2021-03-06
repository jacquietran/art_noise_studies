weave_noise <- function(
  seed, x_min, x_max, y_min, y_max, grid_length, warp_factor, geom_size_min,
  geom_size_max, subsets, noise_type = c("perlin", "simplex", "value", "cubic")){
  
  # Requires {ambient}, {dplyr}, {purrr}
  
  if(missing(x_min)){
    x_min <- 0
  }
  
  if(missing(x_max)){
    x_max <- 1
  }
  
  if(missing(y_min)){
    y_min <- 0
  }
  
  if(missing(y_max)){
    y_max <- 1
  }
  
  if(missing(grid_length)){
    grid_length <- 100
  }
  
  if(missing(geom_size_min)){
    geom_size_min <- 0.2
  }
  
  if(missing(geom_size_max)){
    geom_size_max <- 1.5
  }
  
  if(missing(subsets)){
    subsets <- 20
  }
  
  if(missing(noise_type)){
    noise_type <- "perlin"
  }
  
  if(noise_type == "perlin"){
    
    set.seed(seed)
    grid <- ambient::long_grid(
      seq(x_min, x_max, length.out = grid_length),
      seq(y_min, y_max, length.out = grid_length)) |>
      dplyr::mutate(
        curl = ambient::curl_noise(ambient::gen_perlin, x = x, y = y)) |>
      purrr::reduce(data.frame) |>
      dplyr::rename(x = out, y = elt, curl_x = x, curl_y = y) |>
      dplyr::mutate(
        x_warped = x + (curl_x / warp_factor),
        y_warped = y + (curl_y / warp_factor),
        size = sample(seq(geom_size_min, geom_size_max, by = 0.1),
                      dplyr::n(), replace = TRUE),
        subset = sample(1:subsets, dplyr::n(), replace = TRUE))
    
  }
  
  else if(noise_type == "simplex") {
    
    set.seed(seed)
    grid <- ambient::long_grid(
      seq(x_min, x_max, length.out = grid_length),
      seq(y_min, y_max, length.out = grid_length)) |>
      dplyr::mutate(
        curl = ambient::curl_noise(ambient::gen_simplex, x = x, y = y)) |>
      purrr::reduce(data.frame) |>
      dplyr::rename(x = out, y = elt, curl_x = x, curl_y = y) |>
      dplyr::mutate(
        x_warped = x + (curl_x / warp_factor),
        y_warped = y + (curl_y / warp_factor),
        size = sample(seq(geom_size_min, geom_size_max, by = 0.1),
                      dplyr::n(), replace = TRUE),
        subset = sample(1:subsets, dplyr::n(), replace = TRUE))
    
  }
  
  else if(noise_type == "value") {
    
    set.seed(seed)
    grid <- ambient::long_grid(
      seq(x_min, x_max, length.out = grid_length),
      seq(y_min, y_max, length.out = grid_length)) |>
      dplyr::mutate(
        curl = ambient::curl_noise(ambient::gen_value, x = x, y = y)) |>
      purrr::reduce(data.frame) |>
      dplyr::rename(x = out, y = elt, curl_x = x, curl_y = y) |>
      dplyr::mutate(
        x_warped = x + (curl_x / warp_factor),
        y_warped = y + (curl_y / warp_factor),
        size = sample(seq(geom_size_min, geom_size_max, by = 0.1),
                      dplyr::n(), replace = TRUE),
        subset = sample(1:subsets, dplyr::n(), replace = TRUE))
    
  }
  
  else if(noise_type == "cubic") {
    
    set.seed(seed)
    grid <- ambient::long_grid(
      seq(x_min, x_max, length.out = grid_length),
      seq(y_min, y_max, length.out = grid_length)) |>
      dplyr::mutate(
        curl = ambient::curl_noise(ambient::gen_cubic, x = x, y = y)) |>
      purrr::reduce(data.frame) |>
      dplyr::rename(x = out, y = elt, curl_x = x, curl_y = y) |>
      dplyr::mutate(
        x_warped = x + (curl_x / warp_factor),
        y_warped = y + (curl_y / warp_factor),
        size = sample(seq(geom_size_min, geom_size_max, by = 0.1),
                      dplyr::n(), replace = TRUE),
        subset = sample(1:subsets, dplyr::n(), replace = TRUE))
    
  }
  
  return(grid)
  
}