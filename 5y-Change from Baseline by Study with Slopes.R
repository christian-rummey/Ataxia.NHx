
# . -----------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(lme4)
library(emmeans)
library(officer)
library(rvg)
library(patchwork)

source('.project.settings.R')

# Disable df calculations for large datasets
emm_options(lmerTest.limit = Inf, pbkrtest.limit = Inf)

# FUNCTION: Fit change from baseline mixed model and extract emmeans ----
fit_cfb_model <- function(data, group_var = "study", include_all = TRUE) {
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
fit_slope_model <- function(data, group_var = "study", include_all = TRUE) {
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

      # Get predictions for each study
      studies <- unique(dat[[group_var]][!is.na(dat[[group_var]])])

      slope_predictions <- map_dfr(studies, function(grp) {
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

# FUNCTION: Create single plot for one parameter ----
create_single_plot <- function(data, param, group_var = "study",
                                colors = NULL, y_limits = NULL,
                                show_legend = TRUE, slope_data = NULL) {
  # Default color palette
  if (is.null(colors)) {
    colors <- c(
      "#E41A1C", # red
      "#377EB8", # blue
      "#4DAF4A", # green
      "#984EA3", # purple
      "#FF7F00", # orange
      "#666666"  # dark gray
    )
  }

  # Filter data for this parameter
  plot_data <- data %>% filter(paramcd == param)

  # Create base plot
  p <- plot_data %>%
    ggplot() +
    aes(x = avisitn, y = fitted,
        color = .data[[group_var]],
        group = .data[[group_var]]) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.1)) +
    geom_point(size = 2, position = position_dodge(width = 0.1)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, alpha = 0.7,
                  position = position_dodge(width = 0.1)) +
    scale_color_manual(values = colors) +
    labs(
      x = "Time (years)",
      y = "LS Mean Change from Baseline",
      color = "Study",
      title = param
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    guides(color = guide_legend(nrow = 1)) +
    theme(
      legend.box.spacing = unit(0, "pt"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )

  # Add slope lines if provided
  if (!is.null(slope_data)) {
    slope_plot_data <- slope_data %>% filter(paramcd == param)
    p <- p + geom_line(
      data = slope_plot_data,
      aes(x = tm., y = fitted, color = .data[[group_var]], group = .data[[group_var]]),
      linewidth = 1.2,
      alpha = 0.6,
      linetype = "solid"
    )
  }

  # Apply y-axis limits if provided
  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(xlim = c(0, 5), ylim = y_limits)
  } else {
    p <- p + coord_cartesian(xlim = c(0, 5))
  }

  # Remove legend if requested (for combining plots)
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }

  return(p)
}

# FUNCTION: Plot change from baseline by group ----
plot_cfb_by_group <- function(model_predictions, group_var = "study",
                               colors = NULL, y_limits_list = NULL, slope_predictions = NULL) {
  # Default color palette
  if (is.null(colors)) {
    colors <- c(
      "#E41A1C", # red
      "#377EB8", # blue
      "#4DAF4A", # green
      "#984EA3", # purple
      "#FF7F00", # orange
      "#666666"  # dark gray
    )
  }

  # Get unique parameters
  params <- unique(model_predictions$paramcd)

  # Create individual plots
  plots <- list()
  for (i in seq_along(params)) {
    param <- params[i]
    # Get limits for this parameter if specified
    y_lim <- if (!is.null(y_limits_list)) y_limits_list[[param]] else NULL

    # Only show legend on first plot
    plots[[i]] <- create_single_plot(
      data = model_predictions,
      param = param,
      group_var = group_var,
      colors = colors,
      y_limits = y_lim,
      show_legend = (i == 1),
      slope_data = slope_predictions
    )
  }

  # Combine plots with patchwork
  combined_plot <- wrap_plots(plots, ncol = length(params)) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  return(combined_plot)
}

# FUNCTION: Save plot to PowerPoint ----
save_plot_to_ppt <- function(plot_obj = NULL,
                              title = "title",
                              layout = "1s",
                              i = 2,
                              template = .ppt.template.file,
                              master = "CR",
                              image = FALSE,
                              output_file = NULL) {

  # Get plot object
  if (is.null(plot_obj)) {
    pp <- last_plot()
  } else {
    pp <- plot_obj
  }

  if (is.null(pp)) {
    stop("No plot found. Please create a plot before saving to PowerPoint.")
  }

  # Fix minus signs
  .fix_plot_minuses <- function(p) {
    p + labs(
      title    = gsub("-", "\u2212", p$labels$title),
      subtitle = gsub("-", "\u2212", p$labels$subtitle),
      caption  = gsub("-", "\u2212", p$labels$caption),
      x        = gsub("-", "\u2212", p$labels$x),
      y        = gsub("-", "\u2212", p$labels$y)
    )
  }

  pp <- .fix_plot_minuses(pp)

  # Create output file path
  if (is.null(output_file)) {
    # Use script name as output file name in same directory
    script_path <- sys.frame(1)$ofile
    if (is.null(script_path)) {
      # Fallback to current script
      script_path <- rstudioapi::getSourceEditorContext()$path
    }
    if (is.null(script_path) || script_path == "") {
      # Last resort fallback
      target_file <- .create_output_filepath(title)
    } else {
      output_file <- sub("\\.R$", ".pptx", script_path)
      target_file <- output_file
    }
  } else {
    target_file <- output_file
  }

  # Create PowerPoint
  ppt <- read_pptx(template) %>%
    add_slide(layout = layout, master = master)

  # Add plot as vector or image
  if (!image) {
    ppt <- ppt %>%
      ph_with(
        dml(print(pp, newpage = FALSE)),
        location = ph_location_type(type = "body", type_idx = i)
      )
  } else {
    ppt <- ppt %>%
      ph_with(
        print(pp, newpage = FALSE),
        location = ph_location_type(type = "body", type_idx = i)
      )
  }

  # Add title and notes
  ppt <- ppt %>%
    ph_with(
      title,
      location = ph_location_type(type = "title")
    ) %>%
    set_notes(
      value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
      location = notes_location_type("body")
    ) %>%
    print(target = target_file)

  message("PowerPoint saved to: ", target_file)
  invisible(target_file)
}

# 1. PREPARE DATA ----

# Settings
include_all_group <- F  # Set to FALSE to exclude "all" group from analysis
show_slopes       <- T  # Set to FALSE to skip slope model calculation and display
studies <- c('CRCSCA','EUROSCA')  # Filter to specific studies (NULL = all studies)

# Define y-axis ranges for each parameter (for plotting only)
range.SARA    <- c(0, 8)
range.SARA.ax <- c(0, 3.5)

pars.   <- c('SARA', 'SARA.ap')

dt. <- bind_rows(
  # .dd('adl'),
  .dd('sara'),
  .dd('fars')
) %>%
  filter(avisitn %% 1 == 0) %>%
  filter(paramcd %in% pars.) %>%
  left_join(
    .dd('demo.sca') %>% select(study, sjid, age_bl)
  ) %>%
  select(-stdy)

# Apply study filter if specified
if (!is.null(studies)) {
  dt. <- dt. %>% filter(study %in% studies)
}

dm. <- dt. %>%
  group_by( sjid, paramcd ) %>%
  spread  ( paramcd, aval ) %>%
  filter  ( avisitn == min( avisitn ))

# Create baseline columns for all parameters in pars.
for (par in pars.) {
  if (par %in% names(dm.)) {
    dm.[[paste0(par, ".bl")]] <- dm.[[par]]
  }
}

# Apply baseline filtering if SARA exists
if ("SARA" %in% names(dm.)) {
  dm. <- dm. %>% filter(SARA >= 3)
}

dm. %<>% droplevels()

dt <- dt. %>%
  # join to only keep people in dm., then move on.
  inner_join(
    dm. %>% select(study, sjid),
    by = c("study", "sjid")
  ) %>%
  filter(paramcd != 'fane7') %>%
  group_by( study, sjid, paramcd ) %>%
  filter( tm. < 6 ) %>%
  filter( avisitn < 6) %>%
  filter( n()>1 ) %>%
  arrange( study, sjid, paramcd, tm. ) %>%
  mutate( bl = first(aval) ) %>%
  ungroup() %>%
  droplevels()

# 2. CALCULATE MODELS ----

# Prepare data for modeling
dt_model <- dt %>%
  filter(paramcd %in% pars.)

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

# 3. PLOT GRAPH ----

# Define colors (including black for "all" group)
study_colors <- c(
  "all"     = "#000000", # black for "all"
  "CRCSCA"  = "#E41A1C", # red
  "EUROSCA" = "#377EB8"  # blue
  # Add more studies here as needed
)

y_limits_list <- list(
  SARA = range.SARA,
  SARA.ax = range.SARA.ax
)

# Create combined plot with slopes overlaid
p <- plot_cfb_by_group(
  model_predictions,
  group_var = "study",
  colors = study_colors,
  y_limits_list = y_limits_list,
  slope_predictions = slope_predictions
)

print(p)

# Save to PowerPoint ----
save_plot_to_ppt(
  plot_obj = p,
  title = "5y-Change from Baseline by Study with Slopes",
  layout = "1s",
  i = 2
)
