# Parameter Metadata for Ataxia Natural History Analyses
#
# Contains definitions, ranges, and metadata for clinical outcome parameters

# Y-axis ranges for plotting (used in coord_cartesian)
.param_ranges <- list(
  # FARS subscales
  mFARS = c(0, 20),
  USS = c(0, 10),
  FARS.B = c(0, 30),

  # SARA subscales
  SARA = c(0, 10),
  SARA.ax = c(0, 5),
  SARA.ap = c(0, 8),

  # ADL
  ADL = c(0, 30),

  # Functional staging
  fane7 = c(0, 7)
)

# Parameter labels (for plot titles/axes)
.param_labels <- list(
  mFARS = "mFARS (Modified FARS)",
  USS = "Upright Stability Subscore",
  FARS.B = "FARS Part B",
  SARA = "SARA Total Score",
  SARA.ax = "SARA Axial Subscore",
  SARA.ap = "SARA Appendicular Subscore",
  ADL = "Activities of Daily Living",
  fane7 = "Functional Stage (FA-NESSCA)"
)

# Parameter groupings for related analyses
.param_groups <- list(
  FARS = c('mFARS', 'USS', 'FARS.B'),
  SARA = c('SARA', 'SARA.ax', 'SARA.ap'),
  functional = c('ADL', 'fane7')
)

# Helper function to get range for a parameter
get_param_range <- function(param) {
  if (param %in% names(.param_ranges)) {
    return(.param_ranges[[param]])
  } else {
    warning("No range defined for parameter: ", param)
    return(NULL)
  }
}

# Helper function to get label for a parameter
get_param_label <- function(param) {
  if (param %in% names(.param_labels)) {
    return(.param_labels[[param]])
  } else {
    return(param)  # Return parameter name if no label defined
  }
}
