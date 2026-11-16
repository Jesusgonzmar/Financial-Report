# ==============================================================================
# 04_risk.R - VaR and Expected Shortfall: static, rolling and exceedance check
# ==============================================================================
source("R/00_config.R")
suppressPackageStartupMessages({
  library(PerformanceAnalytics)
  library(zoo)
})

d     <- load_returns()
r     <- as.numeric(d$ret)
alpha <- cfg$alpha
meths <- c(gaussian = "Gaussian", historical = "Historical", modified = "Modified (Cornish-Fisher)")

# --- Static, full-sample, one-day measures ------------------------------------
static <- data.frame(
  Method = unname(meths),
  VaR    = 100 * sapply(names(meths), function(m) as.numeric(VaR(r, p = alpha, method = m))),
  ES     = 100 * sapply(names(meths), function(m) as.numeric(ES(r,  p = alpha, method = m)))
)
names(static)[2:3] <- c("VaR 95% (%)", "ES 95% (%)")
write.csv(static, file.path(cfg$dirs$outputs, "var_es_static.csv"), row.names = FALSE)

# --- Rolling-window estimation ------------------------------------------------
risk_measures <- function(x, p) {
  x <- as.numeric(x)
  c(VaR_gaussian   = as.numeric(VaR(x, p = p, method = "gaussian")),
    VaR_historical = as.numeric(VaR(x, p = p, method = "historical")),
    VaR_modified   = as.numeric(VaR(x, p = p, method = "modified")),
    ES_gaussian    = as.numeric(ES(x,  p = p, method = "gaussian")),
    ES_historical  = as.numeric(ES(x,  p = p, method = "historical")),
    ES_modified    = as.numeric(ES(x,  p = p, method = "modified")))
}

W    <- cfg$roll_window
roll <- rollapply(as.zoo(d$ret), width = W, FUN = risk_measures, p = alpha,
                  by.column = FALSE, align = "right")
write.zoo(roll, file.path(cfg$dirs$outputs, "rolling_risk.csv"), sep = ",", index.name = "Date")

# --- Exceedance check: does r_{t+1} fall below the VaR estimated at t? --------
# Kupiec (1995) proportion-of-failures likelihood-ratio test.
kupiec <- function(x, N, p_exp) {
  pi_hat <- x / N
  ll0 <- (N - x) * log(1 - p_exp) + x * log(p_exp)
  ll1 <- (N - x) * log(1 - pi_hat) + if (x > 0) x * log(pi_hat) else 0
  lr  <- -2 * (ll0 - ll1)
  c(LR = lr, p_value = pchisq(lr, df = 1, lower.tail = FALSE))
}

n   <- length(r)
bt  <- t(sapply(c("VaR_gaussian", "VaR_historical", "VaR_modified"), function(nm) {
  v    <- as.numeric(roll[, nm])             # windows ending at W, ..., n
  viol <- sum(r[(W + 1):n] < v[1:(n - W)])   # next-day returns vs. VaR at t
  N    <- n - W
  c(Exceedances = viol, N = N, Rate = viol / N, Expected = 1 - alpha,
    kupiec(viol, N, 1 - alpha))
}))
backtest <- data.frame(Method = rownames(bt), bt, row.names = NULL)
write.csv(backtest, file.path(cfg$dirs$outputs, "var_backtest_kupiec.csv"), row.names = FALSE)

# --- Figure: returns with rolling VaR and ES ----------------------------------
save_fig("rolling_risk", {
  plot(as.zoo(d$ret) * 100, col = "grey70", xlab = "", ylab = "Per cent",
       main = "Rolling 95% one-day VaR and ES (500-day window)")
  lines(roll[, "VaR_historical"] * 100, col = "steelblue",  lwd = 2)
  lines(roll[, "VaR_modified"]   * 100, col = "darkgreen", lwd = 2, lty = 2)
  lines(roll[, "ES_historical"]  * 100, col = "firebrick", lwd = 2)
  legend("bottomleft", c("Returns", "VaR historical", "VaR modified", "ES historical"),
         col = c("grey70", "steelblue", "darkgreen", "firebrick"),
         lwd = c(1, 2, 2, 2), lty = c(1, 1, 2, 1), bty = "n", cex = 0.85)
})

message("04_risk.R done.")
