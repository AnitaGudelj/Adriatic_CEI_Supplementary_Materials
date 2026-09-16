# =========================================================
# Supplementary material
# Script 03: Figure 9 - histogram of continuous CEI values
# =========================================================
# This script creates a two-panel histogram comparing the distribution of
# positive Composite Exposure Index (CEI) values between summer and winter
# for the 50/50 weighting scenario and 20/60 m depth-threshold setting.
# =========================================================

# ---- Packages ----
required_packages <- c("dplyr", "readr")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(dplyr)
library(readr)

# ---- Paths ----
processed_file <- "data/processed/cei_values_long.csv"
figure_dir <- "outputs/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(processed_file)) {
  stop("Processed CEI file not found: ", processed_file, ". Run scripts/01_exposure_index_and_tables.R first.")
}

# ---- Load and prepare data ----
cei_long <- read_csv(processed_file, show_col_types = FALSE)

histogram_data <- cei_long %>%
  filter(
    depth_thresholds_m == "20/60",
    weighting_scenario == "50_50",
    cei_value > 0
  )

summer_values <- histogram_data %>%
  filter(season == "summer") %>%
  pull(cei_value)

winter_values <- histogram_data %>%
  filter(season == "winter") %>%
  pull(cei_value)

if (length(summer_values) == 0 || length(winter_values) == 0) {
  stop("No positive CEI values found for the selected 50/50 and 20/60 scenario.")
}

x_axis_range <- range(c(summer_values, winter_values), na.rm = TRUE)

# ---- Export TIFF ----
output_tiff <- file.path(figure_dir, "Figure_09_histogram_cei_distribution.tiff")

tiff(
  filename = output_tiff,
  width = 6,
  height = 4.5,
  units = "in",
  res = 600,
  compression = "lzw"
)

par(
  mfrow = c(2, 1),
  family = "serif",
  mar = c(4, 4, 2, 1),
  cex = 1,
  cex.main = 1.1
)

hist(
  summer_values,
  breaks = 50,
  main = "Summer (50/50; 20/60 m)",
  xlab = "Composite Exposure Index",
  ylab = "Frequency",
  xlim = x_axis_range,
  col = "lightblue",
  border = "black"
)

hist(
  winter_values,
  breaks = 50,
  main = "Winter (50/50; 20/60 m)",
  xlab = "Composite Exposure Index",
  ylab = "Frequency",
  xlim = x_axis_range,
  col = "lightgray",
  border = "black"
)

dev.off()

message("Figure exported to: ", output_tiff)
