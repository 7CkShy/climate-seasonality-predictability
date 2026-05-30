calculate_seasonal <- function(
  x,
  start_year = 1990,
  end_year = 2020,
  output_type = "P",
  demo = FALSE,
  s = 10,
  base.binning = 2,
  from = 0.5,
  by = 0.25,
  base.entropy = 2,
  indices.only = FALSE
) {
  output_type <- match.arg(output_type, c("P", "C", "M"))
  fn <- match.fun("mean")

  if (!demo && any(is.na(x))) {
    return(NA)
  }

  if (demo) {
    set.seed(6789)
    df <- tibble::tibble(
      "year" = as.integer(rep(1990:2020, each = 12)),
      "month" = as.integer(rep(1:12, 31)),
      "Q" = runif(372, 0, 1)
    )
  } else {
    years <- seq.int(start_year, end_year)
    expected_n <- length(years) * 12

    if (length(x) != expected_n) {
      stop(
        "Input length does not match start_year/end_year: got ",
        length(x),
        " monthly values, expected ",
        expected_n,
        " for ",
        length(years),
        " years.",
        call. = FALSE
      )
    }

    df <- tibble::tibble(
      "year" = as.integer(rep(years, each = 12)),
      "month" = as.integer(rep(1:12, length(years))),
      "Q" = x
    )
  }

  df$year <- as.factor(df$year)
  df$month <- as.factor(df$month)

  df.monthly <- aggregate(Q ~ month + year, df, fn, na.rm = TRUE)

  df.monthly$Q <- log10(df.monthly$Q + 1)
  df.monthly$class <- cut(
    df.monthly$Q,
    10,
    right = FALSE,
    include.lowest = TRUE
  )
  df.table <- with(df.monthly, table(class, month))

  X <- apply(df.table, 2, sum, na.rm = TRUE)
  Y <- apply(df.table, 1, sum, na.rm = TRUE)
  Z <- sum(df.table, na.rm = TRUE)

  HX <- -1 * sum((X / Z) * log(X / Z, base = base.entropy), na.rm = TRUE)
  HY <- -1 * sum((Y / Z) * log(Y / Z, base = base.entropy), na.rm = TRUE)
  HXY <- -1 *
    sum((df.table / Z) * log(df.table / Z, base = base.entropy), na.rm = TRUE)

  P <- round(1 - (HXY - HX) / log(s, base = base.binning), 2)
  C <- round(1 - HY / log(s, base = base.binning), 2)
  M <- round((HX + HY - HXY) / log(s, base = base.binning), 2)

  if (output_type == "P") {
    return(P)
  } else if (output_type == "M") {
    return(M)
  } else {
    return(C)
  }
}
