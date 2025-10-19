
# . -----------------------------------------------------------------------
# Change from Baseline Analysis by Study with Slope Models
# Compares CRCSCA vs EUROSCA using SARA parameters
# -----------------------------------------------------------------------

rm(list = ls())

source('.settings.R')

# 1. SETTINGS ----

# Analysis settings
include_all_group <- FALSE  # Set to TRUE to include "all" group
show_slopes       <- TRUE   # Set to FALSE to skip slope model calculation

# Data selection
studies    <- c('CRCSCA', 'EUROSCA')
parameters <- c('SARA', 'SARA.ap')

# Baseline filters (applied to dm)
baseline_filters <- list(
  SARA = ">= 3"
)

# Y-axis ranges (use config or specify custom)
y_limits <- list(
  SARA    = get_param_range('SARA'),     # c(0, 8) from config
  SARA.ap = get_param_range('SARA.ap')   # c(0, 8) from config
)

# 2. LOAD DATA ----

data_list <- load_ataxia_data(
  studies = studies,
  parameters = parameters,
  baseline_filters = baseline_filters,
  integer_visits = TRUE,
  time_limit = 6,
  min_visits = 2
)

dt <- data_list$dt
dm <- data_list$dm

# 3. CALCULATE MODELS ----

# Filter to modeling parameters
dt_model <- dt %>%
  filter(paramcd %in% parameters)

# Change from baseline model (discrete visits)
model_predictions <- dt_model %>%
  fit_cfb_model(group_var = "study", include_all = include_all_group)

# Slope model (continuous time) - only if show_slopes is TRUE
if (show_slopes) {
  slope_predictions <- dt_model %>%
    fit_slope_model(group_var = "study", include_all = include_all_group)
} else {
  slope_predictions <- NULL
}

# 4. PLOT ----

# Create combined plot with slopes overlaid
p <- plot_cfb_by_group(
  model_predictions = model_predictions,
  group_var = "study",
  colors = .colors.study,
  y_limits_list = y_limits,
  slope_predictions = slope_predictions
)

print(p)

# 5. SAVE TO POWERPOINT ----

save_plot_to_ppt(
  plot_obj = p,
  title = "5y-Change from Baseline by Study with Slopes",
  layout = "1s",
  i = 2
)
