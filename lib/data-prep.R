# Data Preparation Functions for Ataxia Natural History Analyses
#
# Contains reusable functions for loading and preparing ataxia study data

# FUNCTION: Load and prepare ataxia study data ----
#
# Standardized data loading and preparation for natural history analyses
# Returns dt (long format with baseline) and dm (wide format baseline demographics)
#
# @param studies Character vector of study names (e.g., c('UNIFAI'), c('CRCSCA','EUROSCA'))
# @param parameters Character vector of parameters to include (e.g., c('mFARS','USS'))
# @param demo_source Character string: 'demo.l' for UNIFAI or 'demo.sca' for SCA studies
# @param baseline_filters Named list of baseline filters (e.g., list(SARA = ">= 3", fane7 = "< 5", mFARS = ">= 20"))
# @param age_groups Character vector of age groups to include (NULL = all)
# @param time_limit Numeric; maximum follow-up time in years (default: 6)
# @param integer_visits Logical; if TRUE, filter to integer visit numbers only (default: FALSE)
# @param min_visits Integer; minimum number of visits required per subject (default: 2)
# @return List with two elements: dt (long format data) and dm (baseline demographics)
#
load_ataxia_data <- function(studies,
                              parameters,
                              baseline_filters = NULL,
                              age_groups = NULL,
                              time_limit = 6,
                              integer_visits = FALSE,
                              min_visits = 2) {

  cat("Loading ataxia data...\n")
  cat("  Studies:", paste(studies, collapse = ", "), "\n")
  cat("  Parameters:", paste(parameters, collapse = ", "), "\n")

  # Load and combine data
  dt. <- bind_rows(
    .dd('sara'),
    .dd('fars'),
    .dd('adl')
  )

  # Apply integer visit filter if requested
  if (integer_visits) {
    dt. <- dt. %>% filter(avisitn %% 1 == 0)
  }

  # Filter to requested parameters and studies
  dt. <- dt. %>%
    filter(paramcd %in% parameters) %>%
    filter(study %in% studies)

  # Join demographics - combine both sources to support all studies
  demo_combined <- bind_rows(
    .dd('demo.l') %>% select(study, sjid, age_bl),
    .dd('demo.sca') %>% select(study, sjid, age_bl)
  ) %>%
    distinct(study, sjid, .keep_all = TRUE)

  dt. <- dt. %>%
    left_join(demo_combined) %>%
    select(-stdy)

  cat("  Loaded", n_distinct(dt.$sjid), "subjects\n")

  # Create baseline demographics (wide format)
  dm. <- dt. %>%
    group_by(sjid, paramcd) %>%
    spread(paramcd, aval) %>%
    filter(avisitn == min(avisitn))

  # Dynamically create baseline columns for all parameters
  for (par in parameters) {
    if (par %in% names(dm.)) {
      dm.[[paste0(par, ".bl")]] <- dm.[[par]]
    }
  }

  # Always create age groups
  dm. <- dm. %>%
    mutate(
      age_grp = cut(
        age_bl,
        c(0, 8, 12, 16, 25, 40, 100),
        labels = c('<8y', '8-11y', '12-15y', '16-24y', '25-40y', '>40y'),
        right = TRUE
      )
    ) %>%
    filter(!is.na(age_grp))

  # Apply baseline filters if specified
  if (!is.null(baseline_filters)) {
    for (param in names(baseline_filters)) {
      filter_expr <- baseline_filters[[param]]

      if (param %in% names(dm.)) {
        # Parse filter expression (e.g., ">= 3" or "< 5")
        if (grepl("^>=", filter_expr)) {
          threshold <- as.numeric(sub("^>= *", "", filter_expr))
          dm. <- dm. %>% filter(.data[[param]] >= threshold)
        } else if (grepl("^>", filter_expr)) {
          threshold <- as.numeric(sub("^> *", "", filter_expr))
          dm. <- dm. %>% filter(.data[[param]] > threshold)
        } else if (grepl("^<=", filter_expr)) {
          threshold <- as.numeric(sub("^<= *", "", filter_expr))
          dm. <- dm. %>% filter(.data[[param]] <= threshold)
        } else if (grepl("^<", filter_expr)) {
          threshold <- as.numeric(sub("^< *", "", filter_expr))
          dm. <- dm. %>% filter(.data[[param]] < threshold)
        } else if (grepl("^==", filter_expr)) {
          threshold <- as.numeric(sub("^== *", "", filter_expr))
          dm. <- dm. %>% filter(.data[[param]] == threshold)
        }

        cat("  Applied baseline filter:", param, filter_expr, "\n")
      }
    }
  }

  dm. %<>% droplevels()

  cat("  After baseline filters:", n_distinct(dm.$sjid), "subjects\n")

  # Create final longitudinal dataset
  # Join back to keep only subjects in dm.
  # Always include age_grp since we always create it
  join_cols <- c("study", "sjid", "age_grp")

  dt <- dt. %>%
    inner_join(
      dm. %>% select(all_of(join_cols)),
      by = intersect(join_cols, names(dt.))
    ) %>%
    group_by(study, sjid, paramcd) %>%
    filter(tm. < time_limit) %>%
    filter(avisitn < time_limit) %>%
    filter(n() >= min_visits) %>%
    arrange(study, sjid, paramcd, tm.) %>%
    mutate(bl = first(aval)) %>%
    ungroup() %>%
    droplevels()

  cat("  Final dataset:", n_distinct(dt$sjid), "subjects with", nrow(dt), "observations\n")

  # Filter to requested age groups if specified
  if (!is.null(age_groups)) {
    dt <- dt %>% filter(age_grp %in% age_groups)
    cat("  After age group filter:", n_distinct(dt$sjid), "subjects\n")
  }

  return(list(dt = dt, dm = dm.))
}
