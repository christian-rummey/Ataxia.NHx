# Ataxia.NHx

Common Analyses for Natural History Studies in Ataxias

## Project Structure

```
R.Ataxia.NHx/
├── lib/                    # Shared functions
│   ├── data-prep.R        # load_ataxia_data()
│   ├── models.R           # fit_cfb_model(), fit_slope_model()
│   ├── plotting.R         # Plotting functions
│   └── powerpoint.R       # PowerPoint export
├── config/
│   └── parameters.R       # Parameter ranges, labels, metadata
├── .settings.R            # Project settings (loads all lib/ and config/)
└── [analysis scripts]     # Individual analysis scripts
```

## Quick Start

All scripts follow the same 5-step pattern:

```r
source('.settings.R')  # Loads all libraries and functions

# 1. SETTINGS
studies <- c('UNIFAI')
parameters <- c('mFARS', 'USS')
baseline_filters <- list(mFARS = ">= 20")

# 2. LOAD DATA
data_list <- load_ataxia_data(...)

# 3. CALCULATE MODELS
model_predictions <- fit_cfb_model(...)

# 4. PLOT
p <- plot_cfb_by_group(...)

# 5. SAVE
save_plot_to_ppt(...)
```

## 5-Year Analyses

### Change from Baseline

**5y-Change from Baseline by Age Group.R**
- Study: UNIFAI
- Parameters: mFARS, USS
- Groups: Age groups (<8y, 8-11y, 12-15y, 16-24y, 25-40y, >40y)
- Model: MMRM `aval ~ bl + age_grp * factor(avisitn) + (1 | sjid)`
- Optional slope model: `aval ~ bl + age_grp * tm. + (1 + tm. | sjid)` (toggle `show_slopes`)
- Baseline filters: fane7 < 5, mFARS >= 20

**5y-Change from Baseline by Study.R**
- Studies: CRCSCA, EUROSCA
- Parameters: SARA, SARA.ap
- Groups: Study comparison
- Model: MMRM `aval ~ bl + study * factor(avisitn) + (1 | sjid)`
- Optional slope model (toggle `show_slopes`)
- Baseline filter: SARA >= 3

### Sample Size

**5y-Sample Size by Age Group.R**
- Study: UNIFAI
- Parameters: mFARS, USS
- Two plots: all ages combined, by age group
- Baseline filters: fane7 < 5, mFARS >= 20

**5y-Sample Size by Study.R**
- Studies: CRCSCA, EUROSCA, UNIFAI (configurable)
- Parameters: SARA, SARA.ax
- Two plots: all studies combined, by study
- Baseline filter: SARA >= 3

## Exploratory Analyses

**year-to-year change predictability.R**
- Adjacent 1-year changes: does prior year predict subsequent year?
- Result: noisy, regression-to-mean dominates

**year-to-year change predictability (longer intervals).R**
- Extended version for longer time intervals

**year1_change_classification.R**
- Trajectories stratified by year-1 response (improved/stable/decline)

**change by baseline value.R**
- How baseline severity affects change (mean change by baseline groups)

**change by baseline value (slope model).R**
- Slope model version with random intercepts and slopes

## Other Analyses

**PMA 2025.R**
- Special analysis for PMA conference

**Scales administered by sites-regions.R**
- Cross-sectional view of scale availability by site/region

**conmeds.omav.timeline.R**
- Timeline of omaveloxolone treatment uptake
- Summary statistics by drug type

**conmeds.omav.exposure.R**
- Omaveloxolone exposure analysis

## Shared Functions Reference

### Data Loading

```r
data_list <- load_ataxia_data(
  studies = c('UNIFAI'),
  parameters = c('mFARS', 'USS'),
  baseline_filters = list(mFARS = ">= 20", fane7 = "< 5"),
  age_groups = c('<8y', '8-11y', '12-15y', '16-24y', '25-40y', '>40y'),
  time_limit = 6,
  integer_visits = TRUE,
  min_visits = 2
)
# Returns: list(dt = longitudinal data, dm = baseline demographics)
```

### Statistical Models

```r
# Change from baseline (discrete visits)
model_predictions <- fit_cfb_model(
  data = dt_model,
  group_var = "age_grp",  # or "study"
  include_all = TRUE      # Include "all" group
)

# Slope model (continuous time)
slope_predictions <- fit_slope_model(
  data = dt_model,
  group_var = "age_grp",
  include_all = TRUE
)
```

### Plotting

```r
p <- plot_cfb_by_group(
  model_predictions = model_predictions,
  group_var = "age_grp",
  colors = .colors.age_group,       # or .colors.study
  y_limits_list = list(mFARS = c(0, 20)),
  x_limits = c(0, 5),
  slope_predictions = slope_predictions  # optional
)
```

### PowerPoint Export

```r
save_plot_to_ppt(
  plot_obj = p,
  title = "My Analysis",
  layout = "1s",  # or "TTE"
  i = 2           # body placeholder index
)
```

## Configuration

### Color Palettes

Defined in `.settings.R`:
- `.colors.age_group` - Age group colors
- `.colors.study` - Study colors (CRCSCA, EUROSCA, UNIFAI)

### Parameter Metadata

Defined in `config/parameters.R`:
- `.param_ranges` - Y-axis ranges for each parameter
- `.param_labels` - Display labels
- Helper: `get_param_range('mFARS')` returns `c(0, 20)`

## Notes

- All scripts use `.Rprofile` to load common packages (dplyr, ggplot2, tidyverse, etc.)
- Demographics automatically combined from both `demo.l` and `demo.sca`
- Age groups always created (cut at 8, 12, 16, 25, 40 years)
- Standard filters: time < 6 years, minimum 2 visits
- All PowerPoint outputs named after source script
