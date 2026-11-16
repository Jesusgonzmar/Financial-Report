# Installs the R packages required by the pipeline (only those missing).
pkgs <- c("quantmod", "xts", "zoo", "tseries", "forecast", "TSA",
          "rugarch", "PerformanceAnalytics")

missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}
