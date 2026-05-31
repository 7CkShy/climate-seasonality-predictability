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

# 选取栅格中的一个点进行测试 -----
library(sf)
library(terra)
library(tidyverse)

t_p <- data.frame("id" = 1, "x" = 99, "y" = 38)
t_p <- st_as_sf(t_p, coords = c("x", "y"), crs = 4326)

tif_file <- list.files("./data/qilian_pre_1901_1920/", full.names = TRUE)
r <- rast(tif_file)

p_v <- extract(r, t_p)

p_v <- p_v |>
  pivot_longer(cols = starts_with("pre"), names_to = "time")

p_v$index <- seq_along(p_v$value)

ggplot(data = p_v, mapping = aes(x = index, y = value)) +
  geom_point() +
  geom_line() +
  theme_bw()

# result: 目前得出的结果是在早期的气候数据中，几乎每一年的月变化都是一样的，就会导致M计算的结果非常大，可能是由于早期数据不可直接获取的原因
