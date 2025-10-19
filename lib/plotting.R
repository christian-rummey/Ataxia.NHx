# Plotting Functions for Ataxia Natural History Analyses
#
# Contains reusable plotting functions for change from baseline analyses

library(patchwork)

# FUNCTION: Create single plot for one parameter ----
#
# Creates a single ggplot for one parameter showing change from baseline
# with optional slope model overlay
#
# @param data Data frame with emmeans predictions (from fit_cfb_model)
# @param param Character string specifying which parameter to plot
# @param group_var Character string specifying grouping variable
# @param colors Named vector of colors for groups
# @param y_limits Numeric vector of length 2 (min, max) for y-axis limits
# @param show_legend Logical; whether to show legend
# @param slope_data Data frame with slope predictions (from fit_slope_model), optional
# @return ggplot object
#
create_single_plot <- function(data, param, group_var = "age_grp",
                                colors = NULL, y_limits = NULL, x_limits = NULL,
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
      color = ifelse(group_var == "study", "Study", "Age Group"),
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

  # Apply axis limits if provided
  if (is.null(x_limits)) {
    x_limits <- c(0, 5)  # Default to 5 years
  }

  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(xlim = x_limits, ylim = y_limits)
  } else {
    p <- p + coord_cartesian(xlim = x_limits)
  }

  # Remove legend if requested (for combining plots)
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }

  return(p)
}

# FUNCTION: Plot change from baseline by group ----
#
# Creates combined plot with multiple parameters side-by-side
# using patchwork, with shared legend
#
# @param model_predictions Data frame with emmeans predictions
# @param group_var Character string specifying grouping variable
# @param colors Named vector of colors for groups
# @param y_limits_list Named list of y-axis limits by parameter
# @param slope_predictions Data frame with slope predictions, optional
# @return patchwork combined plot object
#
plot_cfb_by_group <- function(model_predictions, group_var = "age_grp",
                               colors = NULL, y_limits_list = NULL, x_limits = NULL,
                               slope_predictions = NULL) {
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
      x_limits = x_limits,
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
