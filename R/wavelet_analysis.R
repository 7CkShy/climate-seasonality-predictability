# Wavelet-based climate predictability utilities.
#
# This file is intentionally library-first: sourcing it only defines functions.
# Use create_test_raster() and run_wavelet_example() explicitly for demos/tests.

calc_predictability <- function(
  x,
  target_period = 12,
  min_length = target_period * 2,
  do_sig = TRUE
) {
  x <- as.numeric(x)

  if (
    length(x) < min_length ||
      !is.finite(target_period) ||
      target_period <= 0 ||
      any(is.na(x)) ||
      any(!is.finite(x))
  ) {
    return(NA_real_)
  }

  if (stats::sd(x) == 0) {
    return(NA_real_)
  }

  dat <- cbind(time = seq_along(x), value = x)

  wt_res <- tryCatch(
    biwavelet::wt(dat, do.sig = do_sig),
    error = function(e) NULL
  )

  if (
    is.null(wt_res) ||
      is.null(wt_res$period) ||
      is.null(wt_res$signif) ||
      length(wt_res$period) == 0
  ) {
    return(NA_real_)
  }

  period_idx <- which.min(abs(wt_res$period - target_period))
  sig_matrix <- as.matrix(wt_res$signif)

  if (period_idx > nrow(sig_matrix)) {
    return(NA_real_)
  }

  sig_target <- sig_matrix[period_idx, ]
  if (length(sig_target) == 0 || all(is.na(sig_target))) {
    return(NA_real_)
  }

  prop_sig <- sum(sig_target > 1, na.rm = TRUE) / length(x)
  prop_sig <- max(0, min(1, prop_sig))

  as.numeric(prop_sig)
}

create_test_raster <- function(
  ncol = 10,
  nrow = 10,
  nlyr = 60,
  seed = 1,
  target_period = 12,
  noise_sd = 0.3
) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required to create a test raster.", call. = FALSE)
  }

  set.seed(seed)

  n_cells <- ncol * nrow
  time <- seq_len(nlyr)
  seasonal_signal <- sin(2 * pi * time / target_period)

  values <- matrix(NA_real_, nrow = n_cells, ncol = nlyr)
  for (cell_idx in seq_len(n_cells)) {
    amplitude <- stats::runif(1, min = 0.7, max = 1.3)
    offset <- stats::rnorm(1, mean = 0, sd = 0.2)
    noise <- stats::rnorm(nlyr, mean = 0, sd = noise_sd)
    values[cell_idx, ] <- offset + amplitude * seasonal_signal + noise
  }

  r <- terra::rast(ncol = ncol, nrow = nrow, nlyr = nlyr)
  terra::values(r) <- values
  names(r) <- sprintf("month_%03d", seq_len(nlyr))

  r
}

run_wavelet_example <- function(
  ncol = 10,
  nrow = 10,
  nlyr = 60,
  seed = 1,
  target_period = 12,
  cores = 1,
  plot_result = TRUE
) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package 'terra' is required to run the raster example.",
      call. = FALSE
    )
  }

  r_stack <- create_test_raster(
    ncol = ncol,
    nrow = nrow,
    nlyr = nlyr,
    seed = seed,
    target_period = target_period
  )

  predictability_raster <- terra::app(
    r_stack,
    fun = function(values) {
      calc_predictability(values, target_period = target_period)
    },
    cores = cores
  )

  if (isTRUE(plot_result)) {
    terra::plot(
      predictability_raster,
      main = sprintf(
        "Environmental Predictability (%s-month period)",
        target_period
      )
    )
  }

  predictability_raster
}
