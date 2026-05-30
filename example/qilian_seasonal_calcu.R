source("./R/seasonality_calculation.R")

library(sf)
library(terra)
library(dplyr)

# 裁剪数据 ----------------------------------------------------------------------
pre_root <- "./data/China_Pre_1901_2024_from_CRUv4.09"
district_shp <- "./data/ChinaAdminDivisonSHP-master/ChinaAdminDivisonSHP-master/4. District/district.shp"
out_root <- "./data/qilian_pre_1901_1920"

years <- 1901:1920
qilian_adcodes <- c(
  "632221", # Menyuan Hui Autonomous County
  "632222", # Qilian County
  "632223", # Haiyan County
  "632224", # Gangcha County
  "632823" # Tianjun County
)

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
terraOptions(progress = 1)

district <- st_read(district_shp)

qilian <- district |>
  filter(dt_adcode %in% qilian_adcodes)
# st_make_valid() |>
# summarise(region = "qilian")

st_write(qilian, "./data/vect/studu_area.gpkg", layer = "qilian")

if (nrow(qilian) != 1) {
  stop("Failed to build a single Qilian boundary from the district shapefile.")
}

crop_qilian_pre <- function(year) {
  nc_file <- file.path(
    pre_root,
    sprintf("pre_%d", year),
    sprintf("pre_%d.nc", year)
  )
  out_file <- file.path(out_root, sprintf("pre_%d_qilian.tif", year))

  if (!file.exists(nc_file)) {
    warning("Missing precipitation file: ", nc_file)
    return(invisible(NULL))
  }

  pre <- rast(nc_file)
  if (crs(pre) == "") {
    crs(pre) <- "EPSG:4326"
  }

  qilian_vect <- vect(st_transform(qilian, crs(pre)))

  pre_qilian <- pre |>
    crop(qilian_vect) |>
    mask(qilian_vect)

  names(pre_qilian) <- sprintf("pre_%d_%02d", year, seq_len(nlyr(pre_qilian)))

  writeRaster(
    pre_qilian,
    out_file,
    overwrite = TRUE
  )

  message("Saved: ", out_file)
  invisible(out_file)
}

invisible(lapply(years, crop_qilian_pre))

# 计算季节性 ----------------------------------------------------------------------
tif_file <- file.path(out_root, sprintf("pre_%d_qilian.tif", years))
missing_tif <- tif_file[!file.exists(tif_file)]
if (length(missing_tif) > 0) {
  stop("Missing cropped tif files: ", paste(missing_tif, collapse = ", "))
}

pre_stack <- rast(tif_file)
expected_layers <- length(years) * 12
if (nlyr(pre_stack) != expected_layers) {
  stop(
    "Unexpected monthly layer count: got ",
    nlyr(pre_stack),
    ", expected ",
    expected_layers,
    "."
  )
}

C <- app(
  pre_stack,
  calculate_seasonal,
  start_year = min(years),
  end_year = max(years),
  output_type = "C",
  cores = 10
)

M <- app(
  pre_stack,
  calculate_seasonal,
  start_year = min(years),
  end_year = max(years),
  output_type = "M",
  cores = 10
)

sea_r <- C + M
names(sea_r) <- "pre_seasonal"

dir.create("./data/result", recursive = TRUE, showWarnings = FALSE)
writeRaster(sea_r, "./data/result/pre_seasonal.tif", overwrite = TRUE)
