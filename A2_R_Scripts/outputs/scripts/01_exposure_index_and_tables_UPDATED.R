# =========================================================
# Supplementary material
# Script 01: Composite Exposure Index calculation and summary tables
# =========================================================
# This script:
# 1) loads winter and summer input tables;
# 2) calculates the Composite Exposure Index (CEI) for alternative
#    depth-threshold and weighting scenarios;
# 3) classifies CEI values using fixed thresholds and non-zero quartiles;
# 4) calculates and exports non-zero quartile thresholds separately by season;
# 5) exports summary tables and long-format data used by the figure scripts.
#
# Run this script from the project root directory.
# =========================================================

# ---- Packages ----
required_packages <- c("dplyr", "tidyr", "purrr", "readxl", "openxlsx", "readr")
missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(openxlsx)
library(readr)

# ---- Paths ----
input_dir <- "data/raw"
processed_dir <- "data/processed"
table_dir <- "outputs/tables"

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

winter_path <- file.path(input_dir, "winter.xlsx")
summer_path <- file.path(input_dir, "summer.xlsx")

if (!file.exists(winter_path)) stop("Input file not found: ", winter_path)
if (!file.exists(summer_path)) stop("Input file not found: ", summer_path)

# ---- Load input data ----
winter <- read_excel(winter_path)
summer <- read_excel(summer_path)

# Expected input columns:
# id                unique grid-cell identifier
# depth_sea         mean sea depth in the grid cell, positive values in metres
# tr_norm           normalized recreational vessel traffic density, 0-1
# tr_mean           mean recreational vessel traffic density
# nat_area          Natura 2000 area within the grid cell, in m2
# UdioNat_SeaArea0  share of Natura 2000 area in the marine part of the grid cell, 0-1
required_columns <- c("id", "depth_sea", "tr_norm", "tr_mean", "nat_area", "UdioNat_SeaArea0")

missing_winter <- setdiff(required_columns, names(winter))
missing_summer <- setdiff(required_columns, names(summer))

if (length(missing_winter) > 0) {
  stop("Missing columns in winter.xlsx: ", paste(missing_winter, collapse = ", "))
}
if (length(missing_summer) > 0) {
  stop("Missing columns in summer.xlsx: ", paste(missing_summer, collapse = ", "))
}

# ---- CEI calculation ----
calculate_cei <- function(data, shallow_threshold_m, deep_threshold_m) {
  data %>%
    mutate(
      depth_sea = suppressWarnings(as.numeric(depth_sea)),
      shallow_water_score = case_when(
        is.na(depth_sea) ~ 0,
        depth_sea <= shallow_threshold_m ~ 1,
        depth_sea >= deep_threshold_m ~ 0,
        TRUE ~ (deep_threshold_m - depth_sea) / (deep_threshold_m - shallow_threshold_m)
      ),
      cei_50_50 = UdioNat_SeaArea0 * (0.5 * tr_norm + 0.5 * shallow_water_score),
      cei_70_30 = UdioNat_SeaArea0 * (0.7 * tr_norm + 0.3 * shallow_water_score),
      cei_30_70 = UdioNat_SeaArea0 * (0.3 * tr_norm + 0.7 * shallow_water_score)
    )
}

# ---- Classification functions ----
classify_fixed_nonzero <- function(x) {
  out <- rep(NA_character_, length(x))
  out[!is.na(x) & x == 0] <- "No exposure"
  out[!is.na(x) & x > 0     & x < 0.25] <- "Low"
  out[!is.na(x) & x >= 0.25 & x < 0.50] <- "Moderate"
  out[!is.na(x) & x >= 0.50 & x < 0.75] <- "High"
  out[!is.na(x) & x >= 0.75] <- "Critical"
  out
}

classify_quantiles_nonzero <- function(x) {
  out <- rep(NA_character_, length(x))
  out[!is.na(x) & x == 0] <- "No exposure"

  x_positive <- x[!is.na(x) & x > 0]
  if (length(x_positive) == 0) return(out)

  q <- quantile(x_positive, probs = c(0.25, 0.50, 0.75), na.rm = TRUE, type = 7)

  out[!is.na(x) & x > 0    & x <= q[1]] <- "Low"
  out[!is.na(x) & x > q[1] & x <= q[2]] <- "Moderate"
  out[!is.na(x) & x > q[2] & x <= q[3]] <- "High"
  out[!is.na(x) & x > q[3]] <- "Critical"
  out
}

# ---- Prepare combined input table ----
combined_data <- bind_rows(
  winter %>% mutate(season = "winter"),
  summer %>% mutate(season = "summer")
)

depth_thresholds <- tibble(
  shallow_threshold_m = c(10, 20, 20),
  deep_threshold_m = c(50, 50, 60)
)

# ---- Long-format CEI values ----
make_cei_long <- function(data, shallow_threshold_m, deep_threshold_m) {
  calculate_cei(data, shallow_threshold_m, deep_threshold_m) %>%
    select(
      season, id, tr_mean, depth_sea, nat_area, UdioNat_SeaArea0,
      cei_50_50, cei_70_30, cei_30_70
    ) %>%
    pivot_longer(
      cols = c(cei_50_50, cei_70_30, cei_30_70),
      names_to = "weighting_scenario",
      values_to = "cei_value"
    ) %>%
    mutate(
      weighting_scenario = recode(
        weighting_scenario,
        "cei_50_50" = "50_50",
        "cei_70_30" = "70_30",
        "cei_30_70" = "30_70"
      ),
      depth_thresholds_m = paste0(shallow_threshold_m, "/", deep_threshold_m)
    )
}

make_class_summary <- function(data, shallow_threshold_m, deep_threshold_m, classification = c("fixed", "quantile")) {
  classification <- match.arg(classification)

  cei_long <- make_cei_long(data, shallow_threshold_m, deep_threshold_m)

  if (classification == "fixed") {
    cei_long <- cei_long %>%
      mutate(exposure_class = classify_fixed_nonzero(cei_value))
  } else {
    cei_long <- cei_long %>%
      group_by(season, weighting_scenario, depth_thresholds_m) %>%
      mutate(exposure_class = classify_quantiles_nonzero(cei_value)) %>%
      ungroup()
  }

  total_natura_by_season <- cei_long %>%
    distinct(season, id, .keep_all = TRUE) %>%
    group_by(season) %>%
    summarise(total_natura_area_m2 = sum(nat_area, na.rm = TRUE), .groups = "drop")

  cei_long %>%
    group_by(season, depth_thresholds_m, weighting_scenario, exposure_class) %>%
    summarise(
      natura_area_m2 = sum(nat_area, na.rm = TRUE),
      number_of_grid_cells = n(),
      mean_traffic = mean(tr_mean, na.rm = TRUE),
      mean_depth_m = mean(depth_sea, na.rm = TRUE),
      mean_natura_share = mean(UdioNat_SeaArea0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(total_natura_by_season, by = "season") %>%
    mutate(natura_area_percent = 100 * natura_area_m2 / total_natura_area_m2)
}

# ---- Run all scenarios ----
fixed_summary <- pmap_dfr(
  list(depth_thresholds$shallow_threshold_m, depth_thresholds$deep_threshold_m),
  ~ make_class_summary(combined_data, ..1, ..2, classification = "fixed")
)

quantile_summary <- pmap_dfr(
  list(depth_thresholds$shallow_threshold_m, depth_thresholds$deep_threshold_m),
  ~ make_class_summary(combined_data, ..1, ..2, classification = "quantile")
)

cei_values_all <- pmap_dfr(
  list(depth_thresholds$shallow_threshold_m, depth_thresholds$deep_threshold_m),
  ~ make_cei_long(combined_data, ..1, ..2)
)
# =========================================================
# Quartile class for each individual grid cell
# =========================================================
# Quartile classes are assigned separately for winter and summer,
# separately for each weighting scenario and depth-threshold setting.
#
# Classification:
# No exposure : CEI = 0
# Low         : 0 < CEI <= Q25
# Moderate    : Q25 < CEI <= Q50
# High        : Q50 < CEI <= Q75
# Critical    : CEI > Q75
# =========================================================

cei_quartile_classes_by_cell <- cei_values_all %>%
  group_by(
    season,
    depth_thresholds_m,
    weighting_scenario
  ) %>%
  mutate(
    exposure_class = classify_quantiles_nonzero(cei_value)
  ) %>%
  ungroup() %>%
  mutate(
    exposure_class = factor(
      exposure_class,
      levels = c(
        "No exposure",
        "Low",
        "Moderate",
        "High",
        "Critical"
      )
    )
  ) %>%
  arrange(
    depth_thresholds_m,
    weighting_scenario,
    season,
    id
  )


# ---------------------------------------------------------
# Focused table: 50/50 weighting and 20/60 m depth thresholds
# ---------------------------------------------------------

cei_quartile_classes_50_50_20_60 <- cei_quartile_classes_by_cell %>%
  filter(
    depth_thresholds_m == "20/60",
    weighting_scenario == "50_50"
  ) %>%
  arrange(
    factor(season, levels = c("winter", "summer")),
    id
  )


# Check results in R
print(
  cei_quartile_classes_50_50_20_60 %>%
    select(
      season,
      id,
      cei_value,
      exposure_class,
      nat_area,
      depth_sea,
      tr_mean,
      UdioNat_SeaArea0
    )
)

# ---- Quartile thresholds ----
# Quartile thresholds are calculated from NON-ZERO CEI values only.
# They are calculated SEPARATELY for winter and summer, and separately
# for each weighting scenario and depth-threshold setting.
#
# Interpretation:
# No exposure : CEI = 0
# Low         : 0 < CEI <= Q25
# Moderate    : Q25 < CEI <= Q50
# High        : Q50 < CEI <= Q75
# Critical    : CEI > Q75

quartile_thresholds_all <- cei_values_all %>%
  filter(!is.na(cei_value), cei_value > 0) %>%
  group_by(season, depth_thresholds_m, weighting_scenario) %>%
  summarise(
    Q25 = quantile(cei_value, probs = 0.25, na.rm = TRUE, type = 7),
    Q50 = quantile(cei_value, probs = 0.50, na.rm = TRUE, type = 7),
    Q75 = quantile(cei_value, probs = 0.75, na.rm = TRUE, type = 7),
    min_nonzero = min(cei_value, na.rm = TRUE),
    max_value = max(cei_value, na.rm = TRUE),
    n_nonzero = n(),
    .groups = "drop"
  )

# Focused table used for the main quartile-based seasonal hotspot figure:
# 50/50 weighting scenario and 20/60 m depth-threshold setting.
quartile_thresholds_50_50_20_60 <- quartile_thresholds_all %>%
  filter(
    depth_thresholds_m == "20/60",
    weighting_scenario == "50_50"
  ) %>%
  arrange(factor(season, levels = c("winter", "summer")))

print(quartile_thresholds_50_50_20_60)

class_levels <- c("No exposure", "Low", "Moderate", "High", "Critical")
season_levels <- c("winter", "summer")
scenario_levels <- c("30_70", "50_50", "70_30")
threshold_levels <- c("10/50", "20/50", "20/60")

format_summary <- function(data) {
  data %>%
    mutate(
      exposure_class = factor(exposure_class, levels = class_levels),
      season = factor(season, levels = season_levels),
      weighting_scenario = factor(weighting_scenario, levels = scenario_levels),
      depth_thresholds_m = factor(depth_thresholds_m, levels = threshold_levels)
    ) %>%
    arrange(depth_thresholds_m, weighting_scenario, season, exposure_class)
}

fixed_summary <- format_summary(fixed_summary)
quantile_summary <- format_summary(quantile_summary)

# ---- Export results ----
write_csv(fixed_summary, file.path(table_dir, "fixed_classification_summary.csv"))
write_csv(quantile_summary, file.path(table_dir, "quantile_classification_summary.csv"))

# Export quartile thresholds for all seasonal/scenario/depth combinations.
write_csv(
  quartile_thresholds_all,
  file.path(table_dir, "quartile_thresholds_all.csv")
)

# Export the thresholds used for the 50/50 and 20/60 quartile analysis.
# This file contains one row for winter and one row for summer.
write_csv(
  quartile_thresholds_50_50_20_60,
  file.path(table_dir, "quartile_thresholds_50_50_20_60.csv")
)

write_csv(cei_values_all, file.path(processed_dir, "cei_values_long.csv"))

# Backward-compatible filename for older figure scripts.
write_csv(cei_values_all %>% rename(exposure_value = cei_value), file.path(processed_dir, "exposure_values_long.csv"))

write.xlsx(
  list(
    fixed_classification = fixed_summary,
    quantile_classification = quantile_summary,
    quartile_thresholds_all = quartile_thresholds_all,
    quartile_thresholds_50_50_20_60 = quartile_thresholds_50_50_20_60
  ),
  file = file.path(table_dir, "classification_summaries.xlsx"),
  overwrite = TRUE
)
# ---------------------------------------------------------
# Export quartile class for every grid cell
# ---------------------------------------------------------

write_csv(
  cei_quartile_classes_by_cell,
  file.path(
    processed_dir,
    "cei_quartile_classes_by_cell.csv"
  )
)


# Focused output for the main scenario: 50_50 and 20/60
write_csv(
  cei_quartile_classes_50_50_20_60,
  file.path(
    processed_dir,
    "cei_quartile_classes_50_50_20_60.csv"
  )
)

message("Analysis completed. Results exported to data/processed and outputs/tables.")
message("Quartile thresholds exported separately for winter and summer.")
message("Main thresholds file: outputs/tables/quartile_thresholds_50_50_20_60.csv")
