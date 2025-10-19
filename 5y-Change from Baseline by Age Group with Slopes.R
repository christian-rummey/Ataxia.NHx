
# . -----------------------------------------------------------------------
# Change from Baseline Analysis by Age Group with Slope Models
# UNIFAI study - mFARS and USS parameters
# -----------------------------------------------------------------------

rm(list = ls())

source('.settings.R')

# 1. SETTINGS ----

include_all_group <- FALSE
show_slopes       <- F
age_groups        <- c('<8y', '8-11y', '12-15y', '16-24y', '25-40y', '>40y')

studies    <- c('UNIFAI')
parameters <- c('mFARS', 'USS', 'fane7')

baseline_filters <- list(
  fane7 = "< 5",
  mFARS = ">= 20"
)

y_limits <- list(
  mFARS = c(0, 18),
  USS   = c(0, 9)
)

# 2. LOAD DATA ----

data_list <- load_ataxia_data(
  studies = studies,
  parameters = parameters,
  demo_source = 'demo.l',
  baseline_filters = baseline_filters,
  age_groups = age_groups,
  time_limit = 6,
  min_visits = 2
)

dt <- data_list$dt
dm <- data_list$dm

# 3. CALCULATE MODELS ----

dt_model <- dt %>%
  filter(paramcd %in% c('mFARS', 'USS'))

model_predictions <- dt_model %>%
  fit_cfb_model(group_var = "age_grp", include_all = include_all_group)

if (show_slopes) {
  slope_predictions <- dt_model %>%
    fit_slope_model(group_var = "age_grp", include_all = include_all_group)
} else {
  slope_predictions <- NULL
}

# 4. PLOT ----

p <- plot_cfb_by_group(
  model_predictions = model_predictions,
  group_var = "age_grp",
  colors = .colors.age_group,
  y_limits_list = y_limits,
  slope_predictions = slope_predictions
)

print(p)

# 5. SAVE TO POWERPOINT ----

save_plot_to_ppt(
  plot_obj = p,
  title = "5y-Change from Baseline by Age Group with Slopes",
  layout = "1s",
  i = 2
)
