source("R/wavelet_analysis.R")

na_result <- calc_predictability(rep(NA_real_, 60))
stopifnot(length(na_result) == 1L, is.na(na_result))

constant_result <- calc_predictability(rep(1, 60))
stopifnot(length(constant_result) == 1L, is.na(constant_result))

if (!requireNamespace("biwavelet", quietly = TRUE)) {
  message(
    "Skipping wavelet tests because package 'biwavelet' is not installed."
  )
  quit(save = "no", status = 0)
}

time <- seq_len(60)
seasonal_values <- sin(2 * pi * time / 12) + stats::rnorm(60, sd = 0.1)
seasonal_result <- calc_predictability(seasonal_values)
stopifnot(
  length(seasonal_result) == 1L,
  is.numeric(seasonal_result),
  is.na(seasonal_result) || (seasonal_result >= 0 && seasonal_result <= 1)
)

if (!requireNamespace("terra", quietly = TRUE)) {
  message("Skipping raster tests because package 'terra' is not installed.")
  quit(save = "no", status = 0)
}

test_raster <- create_test_raster(ncol = 3, nrow = 2, nlyr = 60, seed = 42)
predictability_raster <- terra::app(test_raster, fun = calc_predictability)

stopifnot(
  terra::nlyr(predictability_raster) == 1L,
  terra::nrow(predictability_raster) == terra::nrow(test_raster),
  terra::ncol(predictability_raster) == terra::ncol(test_raster)
)
