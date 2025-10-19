
# . -----------------------------------------------------------------------
# Sample Size by Study
# CRCSCA vs EUROSCA - SARA parameters
# -----------------------------------------------------------------------

rm(list = ls())

source('.settings.R')

# 1. SETTINGS ----

studies    <- c('CRCSCA', 'EUROSCA','UNIFAI')
parameters <- c('SARA', 'SARA.ax')

baseline_filters <- list(
  SARA = ">= 3"
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

# 3. CALCULATE SAMPLE SIZE ----

sample_size_by_study <- dt %>%
  distinct(study, sjid, avisitn) %>%
  group_by(study, avisitn) %>%
  summarise(n = n(), .groups = 'drop')

sample_size_all <- dt %>%
  distinct(study, sjid, avisitn) %>%
  group_by(avisitn) %>%
  summarise(n = n(), .groups = 'drop')

# 4. PLOT ----

p1 <- sample_size_all %>%
  ggplot() +
  aes(x = avisitn, y = n) +
  geom_bar(stat = "identity", width = 0.7, fill = "#666666") +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +
  scale_x_continuous(breaks = unique(sample_size_all$avisitn)) +
  labs(
    x = "Visit (years)",
    y = "Number of Subjects",
    title = "All Studies"
  ) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  expand_limits(y = max(sample_size_all$n) * 1.15)

p2 <- sample_size_by_study %>%
  ggplot() +
  aes(x = avisitn, y = n, fill = study) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = n, group = study),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  scale_fill_manual(values = .colors.study) +
  scale_x_continuous(breaks = unique(sample_size_by_study$avisitn)) +
  labs(
    x = "Visit (years)",
    y = "Number of Subjects",
    fill = "Study",
    title = "By Study"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  expand_limits(y = max(sample_size_by_study$n) * 1.15)

print(p1)
print(p2)

# 5. SAVE TO POWERPOINT ----

script_path <- rstudioapi::getSourceEditorContext()$path
if (is.null(script_path) || script_path == "") {
  output_file <- .create_output_filepath("Sample Size by Study")
} else {
  output_file <- sub("\\.R$", ".pptx", script_path)
}

ppt <- read_pptx(.ppt.template.file)

ppt <- ppt %>%
  add_slide(layout = "TTE", master = "CR") %>%
  ph_with(
    dml(print(p1, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Sample Size - All Studies",
    location = ph_location_type(type = "title")
  )

ppt <- ppt %>%
  add_slide(layout = "TTE", master = "CR") %>%
  ph_with(
    dml(print(p2, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Sample Size - By Study",
    location = ph_location_type(type = "title")
  )

print(ppt, target = output_file)

message("PowerPoint saved to: ", output_file)
