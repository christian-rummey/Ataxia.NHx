
# . -----------------------------------------------------------------------

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(officer)
library(rvg)

source('.project.settings.R')

# 1. PREPARE DATA ----

# Settings
studies <- c('CRCSCA','EUROSCA')  # Filter to specific studies (NULL = all studies)
age_groups <- NULL  # Filter to specific age groups (NULL = all groups)

pars.   <- c('SARA','SARA.ax')

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
  filter  ( avisitn == min( avisitn )) %>%
  mutate  ( SARA.bl = SARA, SARA.ax.bl = SARA.ax ) 

# %>%
#   # mutate  ( age_grp = cut(age_bl, c(0,8,12,16,25,40,100), labels = c('<8y', '8-11y', '12-15y', '16-24y', '25-40y', '>40y' ), right = T)) %>%
#   filter  ( !is.na(age_grp))

dm. %<>%
  # filter( fane7.bl < 5   ) %>%
  filter( SARA >= 3 ) %>%
  droplevels()

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

# Apply age group filter if specified
# if (!is.null(age_groups)) {
#   dt <- dt %>% filter(age_grp %in% age_groups)
# }

# 2. CALCULATE SAMPLE SIZE ----

# Count unique subjects per visit and study
sample_size_by_study <- dt %>%
  distinct(study, sjid, avisitn) %>%
  group_by(study, avisitn) %>%
  summarise(n = n(), .groups = 'drop')

# Count unique subjects per visit (all studies combined)
sample_size_all <- dt %>%
  distinct(study, sjid, avisitn) %>%
  group_by(avisitn) %>%
  summarise(n = n(), .groups = 'drop')

# 3. PLOT GRAPHS ----

# Define colors by study
study_colors <- c(
  "CRCSCA"  = "#E41A1C",  # red
  "EUROSCA" = "#377EB8"   # blue
  # Add more studies here as needed
  # "STUDY3" = "#4DAF4A"   # green
)

# Plot 1: All studies combined
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
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  expand_limits(y = max(sample_size_all$n) * 1.15)  # Add space for labels

print(p1)

# Plot 2: By study
p2 <- sample_size_by_study %>%
  ggplot() +
  aes(x = avisitn, y = n, fill = study) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = n, group = study),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  scale_fill_manual(values = study_colors) +
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
  expand_limits(y = max(sample_size_by_study$n) * 1.15)  # Add space for labels

print(p2)

# 4. SAVE TO POWERPOINT ----

# Save both plots to PowerPoint (two slides in one file)
# Create output file path
script_path <- rstudioapi::getSourceEditorContext()$path
if (is.null(script_path) || script_path == "") {
  output_file <- .create_output_filepath("Sample Size by Study")
} else {
  output_file <- sub("\\.R$", ".pptx", script_path)
}

# Create PowerPoint with two slides
ppt <- read_pptx(.ppt.template.file)

# Slide 1: All studies
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

# Slide 2: By study
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

# Save file
print(ppt, target = output_file)

message("PowerPoint saved to: ", output_file)
