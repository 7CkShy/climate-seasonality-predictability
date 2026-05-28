caculate_seasonal <- function() {}

set.seed(6789)
df <- data.frame(
  "year" = as.integer(rep(1990:2020, each = 12)),
  "month" = as.integer(rep(1:12, 31)),
  "Q" = runif(372, 0, 1)
)

df$year <- as.factor(df$year)
df$month <- as.factor(df$month)

df.monthly <- aggregate(Q ~ month + year, df, "mean", na.rm = TRUE)

df.monthly$Q <- log10(df.monthly$Q + 1)
df.monthly$class <- cut(df.monthly$Q, 10, right = FALSE, include.lowest = TRUE)
df.table <- with(df.monthly, table(class, month))

X <- apply(df.table, 2, sum, na.rm = TRUE)
Y <- apply(df.table, 1, sum, na.rm = TRUE)
Z <- sum(df.table, na.rm = TRUE)

HX <- -1 * sum((X / Z) * log(X / Z, base = 2), na.rm = TRUE)
HY <- -1 * sum((Y / Z) * log(Y / Z, base = 2), na.rm = TRUE)
HXY <- -1 * sum((df.table / Z) * log(df.table / Z, base = 2), na.rm = TRUE)

P <- round(1 - (HXY - HX) / log(10, base = 2), 2)
C <- round(1 - HY / log(10, base = 2), 2)
M <- round((HX + HY - HXY) / log(10, base = 2), 2)
