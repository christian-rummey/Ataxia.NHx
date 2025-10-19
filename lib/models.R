# Statistical Models for Ataxia Natural History Analyses
#
# Contains reusable model functions for change from baseline analyses

library(lme4)
library(emmeans)

# Disable df calculations for large datasets
emm_options(lmerTest.limit = Inf, pbkrtest.limit = Inf)

# FUNCTION: Fit change from baseline mixed model and extract emmeans ----
#
# Fits MMRM (mixed model for repeated measures) with baseline as covariate
# and extracts estimated marginal means for each group x visit combination
#
# @param data Data frame with columns: paramcd, sjid, avisitn, aval, bl, and group_var
# @param group_var Character string specifying grouping variable (e.g., "age_grp", "study")
# @param include_all Logical; if TRUE, also fits model without group stratification
# @return Data frame with emmeans predictions (fitted, SE, lower, upper) by paramcd, group, visit
#
# Model: aval ~ bl + group_var * factor(avisitn) + (1 | sjid)
# For "all" group: aval ~ bl + factor(avisitn) + (1 | sjid)
#
fit_cfb_model <- function(data, group_var = "age_grp", include_all = TRUE) {
  data %>%
    split(.$paramcd) %>%
    map_dfr(function(dat) {
      param_name <- unique(dat$paramcd)
      cat("Fitting model for", param_name, "...\n")

      # Separate baseline and post-baseline data
      dat_post <- dat %>% filter(avisitn > 0)
      baseline_mean <- mean(dat$bl, na.rm = TRUE)

      # Change from baseline model with baseline as covariate
      # aval ~ bl + group_var + avisitn + group_var:avisitn
      formula_str <- paste0("aval ~ bl + ", group_var, " * factor(avisitn) + (1 | sjid)")
      mod <- lmer(as.formula(formula_str), data = dat_post)
      cat("  Model fitted. Extracting emmeans...\n")

      # Get emmeans for each group x visit combination
      em_formula <- as.formula(paste0("~ ", group_var, " | avisitn"))
      em <- emmeans(mod, em_formula, at = list(bl = baseline_mean))

      # Convert to data frame and calculate change from baseline
      em_df <- as.data.frame(em) %>%
        mutate(
          paramcd = unique(dat$paramcd),
          fitted = emmean - baseline_mean,
          lower = emmean - 1.96 * SE - baseline_mean,
          upper = emmean + 1.96 * SE - baseline_mean
        ) %>%
        select(paramcd, all_of(group_var), avisitn, fitted, SE, lower, upper)

      # Add baseline (avisitn = 0) with change = 0
      baseline_df <- dat %>%
        distinct(across(all_of(group_var))) %>%
        filter(!is.na(.data[[group_var]])) %>%
        mutate(
          paramcd = unique(dat$paramcd),
          avisitn = 0,
          fitted = 0,
          SE = 0,
          lower = 0,
          upper = 0
        )

      # Combine baseline and post-baseline
      result <- bind_rows(baseline_df, em_df)

      # Add "all" group if requested
      if (include_all) {
        # Fit model without group variable for overall estimate
        formula_all <- "aval ~ bl + factor(avisitn) + (1 | sjid)"
        mod_all <- lmer(as.formula(formula_all), data = dat_post)

        em_all <- emmeans(mod_all, ~ avisitn, at = list(bl = baseline_mean))

        em_all_df <- as.data.frame(em_all) %>%
          mutate(
            paramcd = unique(dat$paramcd),
            fitted = emmean - baseline_mean,
            lower = emmean - 1.96 * SE - baseline_mean,
            upper = emmean + 1.96 * SE - baseline_mean
          ) %>%
          select(paramcd, avisitn, fitted, SE, lower, upper)

        # Add group variable column
        em_all_df[[group_var]] <- factor("all")

        # Add baseline for "all"
        baseline_all <- data.frame(
          paramcd = unique(dat$paramcd),
          avisitn = 0,
          fitted = 0,
          SE = 0,
          lower = 0,
          upper = 0
        )
        baseline_all[[group_var]] <- factor("all")

        result <- bind_rows(result, baseline_all, em_all_df)
      }

      return(result)
    })
}

# FUNCTION: Fit slope model with random intercepts and slopes ----
#
# Fits mixed effects model with continuous time and random slopes
# to estimate smooth trajectories over time
#
# @param data Data frame with columns: paramcd, sjid, tm., aval, bl, and group_var
# @param group_var Character string specifying grouping variable
# @param include_all Logical; if TRUE, also fits model without group stratification
# @return Data frame with smooth predictions over continuous time (50 points)
#
# Model: aval ~ bl + group_var * tm. + (1 + tm. | sjid)
# For "all" group: aval ~ bl + tm. + (1 + tm. | sjid)
#
fit_slope_model <- function(data, group_var = "age_grp", include_all = TRUE) {
  data %>%
    split(.$paramcd) %>%
    map_dfr(function(dat) {
      param_name <- unique(dat$paramcd)
      cat("Fitting slope model for", param_name, "...\n")

      # Use all data including baseline (tm. starting from 0)
      baseline_mean <- mean(dat$bl, na.rm = TRUE)

      # Slope model with random intercepts and slopes
      # aval ~ bl + group_var + tm. + group_var:tm. + (1 + tm. | sjid)
      formula_str <- paste0("aval ~ bl + ", group_var, " * tm. + (1 + tm. | sjid)")
      mod <- lmer(as.formula(formula_str), data = dat)
      cat("  Slope model fitted. Extracting predictions...\n")

      # Create prediction grid for smooth lines
      tm_seq <- seq(0, max(dat$tm., na.rm = TRUE), length.out = 50)
      pred_grid <- expand.grid(
        tm. = tm_seq,
        stringsAsFactors = FALSE
      )
      pred_grid$bl <- baseline_mean

      # Get predictions for each group
      groups <- unique(dat[[group_var]][!is.na(dat[[group_var]])])

      slope_predictions <- map_dfr(groups, function(grp) {
        pred_grid[[group_var]] <- grp

        # Get fixed effects predictions
        pred <- predict(mod, newdata = pred_grid, re.form = NA)

        pred_grid %>%
          mutate(
            paramcd = param_name,
            !!group_var := grp,
            fitted = pred - baseline_mean,  # Convert to change from baseline
            model_type = "slope"
          )
      })

      # Add "all" group if requested
      if (include_all) {
        formula_all <- "aval ~ bl + tm. + (1 + tm. | sjid)"
        mod_all <- lmer(as.formula(formula_all), data = dat)

        pred_grid_all <- data.frame(
          tm. = tm_seq,
          bl = baseline_mean
        )

        pred_all <- predict(mod_all, newdata = pred_grid_all, re.form = NA)

        slope_all <- pred_grid_all %>%
          mutate(
            paramcd = param_name,
            fitted = pred_all - baseline_mean,
            model_type = "slope"
          )
        slope_all[[group_var]] <- factor("all")

        slope_predictions <- bind_rows(slope_predictions, slope_all)
      }

      return(slope_predictions)
    })
}
