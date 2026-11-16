# ==============================================================================
# 02_mean_models.R - ARMA / auto.arima / NNAR and out-of-sample comparison
# ==============================================================================
source("R/00_config.R")
suppressPackageStartupMessages(library(forecast))

d     <- load_returns()
train <- ts(as.numeric(d$train))
test  <- as.numeric(d$test)
h     <- length(test)

# --- Linear models ------------------------------------------------------------
m_ma1  <- Arima(train, order = c(0, 0, 1), method = "ML")
m_arma <- Arima(train, order = c(1, 0, 1), method = "ML")
m_auto <- auto.arima(train, seasonal = FALSE)

# --- Non-linear model: neural-network autoregression --------------------------
set.seed(cfg$seed)
m_nn <- nnetar(train)

# --- Out-of-sample (multi-step) forecasts over the whole test sample ----------
fc_arma <- forecast(m_arma, h = h)
fc_auto <- forecast(m_auto, h = h)
fc_nn   <- forecast(m_nn,   h = h)

rmse <- function(e) sqrt(mean(e^2))
mae  <- function(e) mean(abs(e))

order_label <- function(m) {
  o <- arimaorder(m)
  sprintf("ARMA(%d,%d)", o["p"], o["q"])
}

comparison <- data.frame(
  Model = c("MA(1)", "ARMA(1,1)", paste0("auto.arima: ", order_label(m_auto)),
            sprintf("NNAR(%d,%d)", m_nn$p, m_nn$size), "Zero forecast"),
  AIC   = c(AIC(m_ma1), AIC(m_arma), AIC(m_auto), NA, NA),
  RMSE  = c(rmse(test - forecast(m_ma1, h = h)$mean),
            rmse(test - fc_arma$mean),
            rmse(test - fc_auto$mean),
            rmse(test - fc_nn$mean),
            rmse(test)),
  MAE   = c(mae(test - forecast(m_ma1, h = h)$mean),
            mae(test - fc_arma$mean),
            mae(test - fc_auto$mean),
            mae(test - fc_nn$mean),
            mae(test))
)
comparison$RMSE <- 100 * comparison$RMSE   # percentage points
comparison$MAE  <- 100 * comparison$MAE
names(comparison)[3:4] <- c("RMSE (%)", "MAE (%)")

write.csv(comparison, file.path(cfg$dirs$outputs, "model_comparison.csv"), row.names = FALSE)
write_tex_table(comparison, file.path(cfg$dirs$tables, "model_comparison.tex"), digits = 4)

# --- Figure: forecasts against the realised test sample -----------------------
save_fig("forecast_comparison", {
  rng <- range(100 * test)
  plot(100 * test, type = "l", col = "grey60", ylim = rng,
       xlab = "Test-sample day", ylab = "Log return (%)",
       main = "Out-of-sample forecasts vs. realised returns")
  lines(100 * as.numeric(fc_arma$mean), col = "steelblue", lwd = 2)
  lines(100 * as.numeric(fc_nn$mean),   col = "firebrick", lwd = 2)
  legend("topright", c("Realised", "ARMA(1,1)", "NNAR"),
         col = c("grey60", "steelblue", "firebrick"), lwd = c(1, 2, 2), bty = "n")
})

message("02_mean_models.R done.")
