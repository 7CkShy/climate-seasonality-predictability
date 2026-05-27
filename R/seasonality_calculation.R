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

X <- apply(df.table, 2, sum, na.rm = T)
Y <- apply(df.table, 1, sum, na.rm = T)
Z <- sum(df.table, na.rm = T)
