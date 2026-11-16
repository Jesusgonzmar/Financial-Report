# ==============================================================================
# 01_descriptive.R - Moments, normality, stationarity and serial dependence
# ==============================================================================
source("R/00_config.R")
suppressPackageStartupMessages({
  library(PerformanceAnalytics)
  library(zoo)
})

d <- load_returns()
r <- as.numeric(d$ret)

# --- Descriptive statistics ---------------------------------------------------
desc <- data.frame(
  Statistic = c("Observations", "Mean (%)", "Std. deviation (%)", "Skewness",
                "Excess kurtosis", "Minimum (%)", "Maximum (%)"),
  Value     = c(length(r), 100 * mean(r), 100 * sd(r),
                PerformanceAnalytics::skewness(r),
                PerformanceAnalytics::kurtosis(r),   # excess kurtosis by default
                100 * min(r), 100 * max(r))
)
write.csv(desc, file.path(cfg$dirs$outputs, "descriptive_stats.csv"), row.names = FALSE)
write_tex_table(desc, file.path(cfg$dirs$tables, "descriptive_stats.tex"), digits = 3)

# --- Hypothesis tests ---------------------------------------------------------
tt  <- t.test(r)                               # H0: mean = 0
jb  <- tseries::jarque.bera.test(r)            # H0: normality
adf <- tseries::adf.test(r)                    # H0: unit root
lb1 <- Box.test(r,   lag = 10, type = "Ljung-Box")
lb2 <- Box.test(r^2, lag = 10, type = "Ljung-Box")

tests <- data.frame(
  Test      = c("t-test (H0: mean = 0)", "Jarque-Bera (H0: normality)",
                "Augmented Dickey-Fuller (H0: unit root)",
                "Ljung-Box, returns (lag 10)", "Ljung-Box, squared returns (lag 10)"),
  Statistic = c(unname(tt$statistic), unname(jb$statistic), unname(adf$statistic),
                unname(lb1$statistic), unname(lb2$statistic)),
  p_value   = c(tt$p.value, jb$p.value, adf$p.value, lb1$p.value, lb2$p.value)
)
write.csv(tests, file.path(cfg$dirs$outputs, "tests.csv"), row.names = FALSE)

# --- Candidate ARMA orders (EACF); printed table saved as text ----------------
writeLines(capture.output(TSA::eacf(r, ar.max = 7, ma.max = 7)),
           file.path(cfg$dirs$outputs, "eacf.txt"))

# --- Figures ------------------------------------------------------------------
save_fig("returns_series", {
  plot(as.zoo(d$ret) * 100, xlab = "", ylab = "Log return (%)",
       main = "AMZN daily log returns", col = "grey30")
  abline(h = 0, col = "firebrick", lty = 2)
})

save_fig("return_distribution", {
  par(mfrow = c(1, 2))
  hist(100 * r, breaks = 50, freq = FALSE, col = "grey85", border = "white",
       main = "Histogram vs. normal", xlab = "Log return (%)")
  curve(dnorm(x, mean(100 * r), sd(100 * r)), add = TRUE, col = "firebrick", lwd = 2)
  qqnorm(r, main = "Normal Q-Q plot", pch = 16, cex = 0.5, col = "grey30")
  qqline(r, col = "firebrick", lwd = 2)
}, height = 4)

save_fig("acf_returns", {
  par(mfrow = c(1, 2))
  acf(r,   main = "ACF of returns")
  acf(r^2, main = "ACF of squared returns")
}, height = 4)

message("01_descriptive.R done.")
