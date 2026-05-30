source("./R/wavelet_analysis.R")
source("./R/seasonality_calculation.R")

test_raster <- create_test_raster(
  ncol = 360,
  nrow = 180,
  nlyr = 72,
  seed = 42
) +
  2
seasonal_raster_m <- terra::app(
  test_raster,
  fun = calculate_seasonal,
  start_year = 1,
  end_year = 6,
  output_type = "M",
  cores = 4
)

seasonal_raster_c <- terra::app(
  test_raster,
  fun = calculate_seasonal,
  start_year = 1,
  end_year = 6,
  output_type = "C",
  cores = 4
)
