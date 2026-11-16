# ==============================================================================
# 03_garch.R - ARCH-effect diagnostics and ARMA(1,1)-sGARCH(1,1) with sstd
# ==============================================================================
source("R/00_config.R")
suppressPackageStartupMessages({
  library(rugarch)
  library(zoo)
})

d     <- load_returns()
train <- d$train

# --- 1. ARCH effects in the residuals of the ARMA(1,1) mean model -------------
arma_res <- residuals(stats::arima(as.numeric(train), order = c(1, 0, 1)))
arch_pre <- arch_lm(arma_res)
write.csv(arch_pre, file.path(cfg$dirs$outputs, "arch_lm_arma_residuals.csv"), row.names = FALSE)

# --- 2. ARMA(1,1)-sGARCH(1,1) with skewed Student-t innovations ---------------
spec <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(1, 1), include.mean = TRUE),
  distribution.model = "sstd"
)
fit <- ugarchfit(spec = spec, data = train)
if (convergence(fit) != 0) warning("GARCH optimiser did not converge.")

writeLines(capture.output(show(fit)), file.path(cfg$dirs$outputs, "garch_summary.txt"))

mc    <- fit@fit$matcoef
coefs <- data.frame(Parameter = rownames(mc), mc, row.names = NULL, check.names = FALSE)
write.csv(coefs, file.path(cfg$dirs$outputs, "garch_coefficients.csv"), row.names = FALSE)

# --- 3. Persistence, half-life and a test of symmetry (H0: skew = 1) ----------
cf          <- coef(fit)
persistence <- unname(cf["alpha1"] + cf["beta1"])
skew_z      <- (mc["skew", "Estimate"] - 1) / mc["skew", "Std. Error"]
key <- data.frame(
  Quantity = c("alpha1", "beta1", "alpha1 + beta1", "Half-life of a shock (days)",
               "sstd shape (degrees of freedom)", "sstd skew (xi)",
               "z-statistic for H0: skew = 1", "p-value for H0: skew = 1"),
  Value    = c(cf["alpha1"], cf["beta1"], persistence,
               log(0.5) / log(persistence), cf["shape"], cf["skew"],
               skew_z, 2 * pnorm(-abs(skew_z)))
)
write.csv(key, file.path(cfg$dirs$outputs, "garch_key_quantities.csv"), row.names = FALSE)

# --- 4. Diagnostics on standardised residuals ---------------------------------
z        <- as.numeric(residuals(fit, standardize = TRUE))
arch_pos <- arch_lm(z)
lb       <- sapply(c(10, 20), function(l) Box.test(z^2, lag = l, type = "Ljung-Box")$p.value)
diag_tab <- rbind(
  data.frame(Test = paste0("ARCH-LM (lag ", arch_pos$lag, ")"), p_value = arch_pos$p_value),
  data.frame(Test = paste0("Ljung-Box on squared residuals (lag ", c(10, 20), ")"), p_value = lb)
)
write.csv(diag_tab, file.path(cfg$dirs$outputs, "garch_diagnostics.csv"), row.names = FALSE)

# --- 5. Figure: conditional volatility ----------------------------------------
save_fig("conditional_volatility", {
  vol <- as.zoo(sigma(fit)) * 100
  plot(abs(as.zoo(train)) * 100, col = "grey75", xlab = "", ylab = "Per cent",
       main = "Conditional volatility from ARMA(1,1)-sGARCH(1,1)")
  lines(vol, col = "steelblue", lwd = 2)
  legend("topleft", c("|return|", "Conditional s.d."),
         col = c("grey75", "steelblue"), lwd = c(1, 2), bty = "n")
})

message("03_garch.R done.")
