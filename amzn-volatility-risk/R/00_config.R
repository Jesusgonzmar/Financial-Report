# ==============================================================================
# 00_config.R - Project configuration and shared helpers
# All scripts must be run from the repository root, e.g. `Rscript R/run_all.R`.
# ==============================================================================

cfg <- list(
  ticker      = "AMZN",
  from        = as.Date("2019-01-01"),
  to          = as.Date("2021-12-31"),
  train_share = 0.80,    # chronological train/test split
  alpha       = 0.95,    # VaR / ES confidence level
  roll_window = 500L,    # rolling-window length (trading days)
  seed        = 2026L,
  dirs        = list(data    = "data",
                     outputs = "outputs",
                     figures = "report/figures",
                     tables  = "report/tables")
)

invisible(lapply(cfg$dirs, dir.create, showWarnings = FALSE, recursive = TRUE))
set.seed(cfg$seed)   # re-seeded every time a script sources this file

# ------------------------------------------------------------------------------
# Data: download once from Yahoo Finance, then read from the local cache
# ------------------------------------------------------------------------------
load_returns <- function() {
  suppressPackageStartupMessages({
    library(quantmod)
    library(xts)
  })
  cache <- file.path(cfg$dirs$data, "amzn_adjusted_close.csv")

  if (file.exists(cache)) {
    df  <- read.csv(cache, stringsAsFactors = FALSE)
    adj <- xts(df$AdjClose, order.by = as.Date(df$Date))
  } else {
    message("Downloading ", cfg$ticker, " from Yahoo Finance ...")
    px  <- getSymbols(cfg$ticker, src = "yahoo", from = cfg$from,
                      to = cfg$to, auto.assign = FALSE)
    adj <- Ad(px)
    write.csv(data.frame(Date = index(adj), AdjClose = as.numeric(adj)),
              cache, row.names = FALSE)
  }
  colnames(adj) <- "AdjClose"

  ret <- na.omit(diff(log(adj)))          # r_t = ln(P_t / P_{t-1})
  colnames(ret) <- "ret"

  n_train <- floor(cfg$train_share * nrow(ret))
  list(price = adj,
       ret   = ret,
       train = ret[seq_len(n_train)],
       test  = ret[-seq_len(n_train)])
}

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# Save a base-graphics figure as a 300-dpi PNG in report/figures
save_fig <- function(name, expr, width = 8, height = 4.5) {
  png(file.path(cfg$dirs$figures, paste0(name, ".png")),
      width = width, height = height, units = "in", res = 300)
  on.exit(dev.off())
  expr
}

# Write a data frame as a booktabs `tabular` fragment for \input{} in LaTeX
write_tex_table <- function(df, file, digits = 4, align = NULL) {
  esc <- function(s) gsub("_", "\\\\_", gsub("%", "\\\\%", s))
  fmt <- function(x) {
    out <- if (is.numeric(x)) formatC(x, format = "f", digits = digits) else esc(as.character(x))
    out[is.na(x) | out == "NA"] <- "--"
    out
  }
  cells <- lapply(df, fmt)
  rows  <- do.call(paste, c(cells, sep = " & "))
  if (is.null(align)) align <- paste0("l", strrep("r", ncol(df) - 1))
  writeLines(c(
    sprintf("\\begin{tabular}{%s}", align),
    "\\toprule",
    paste0(paste(esc(names(df)), collapse = " & "), " \\\\"),
    "\\midrule",
    paste0(rows, " \\\\"),
    "\\bottomrule",
    "\\end{tabular}"
  ), file)
}

# Engle's ARCH-LM test: regress x^2 on its own q lags; LM = n * R^2 ~ chi2(q)
arch_lm <- function(x, lags = c(4, 8, 12, 16)) {
  z <- as.numeric(x)^2
  res <- t(sapply(lags, function(q) {
    X    <- embed(z, q + 1)
    r2   <- summary(lm(X[, 1] ~ X[, -1]))$r.squared
    stat <- nrow(X) * r2
    c(lag = q, LM = stat, p_value = pchisq(stat, df = q, lower.tail = FALSE))
  }))
  as.data.frame(res)
}
