# ==============================================================================
# run_all.R - Run the whole pipeline in order. Usage: Rscript R/run_all.R
# ==============================================================================
stopifnot("Run this script from the repository root" = file.exists("R/00_config.R"))
source("R/00_config.R")

scripts <- c("01_descriptive.R", "02_mean_models.R", "03_garch.R", "04_risk.R")
for (s in scripts) {
  message("\n=== ", s, " ===")
  source(file.path("R", s), echo = FALSE)
}

writeLines(capture.output(sessionInfo()),
           file.path(cfg$dirs$outputs, "session_info.txt"))
message("\nPipeline finished. Outputs in '", cfg$dirs$outputs, "', figures and tables in 'report/'.")
