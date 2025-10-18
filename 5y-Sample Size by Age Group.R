
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
age_groups        <- c('<8y', '8-11y', '12-15y', '16-24y', '25-40y', '>40y')  # Filter to specific age groups (NULL = all groups)

pars.   <- c('mFARS','USS', 'fane7')
studies <- c('UNIFAI')

dt. <- bind_rows(
  .dd('sara'),
  .dd('fars'),
  .dd('adl')
) %>%
  filter(paramcd %in% pars.) %>%
  filter(study %in% studies) %>%
  left_join(
    .dd('demo.l') %>% select(study, sjid, age_bl)
  ) %>%
  select(-stdy)

dm. <- dt. %>%
  group_by( sjid, paramcd ) %>%
  spread  ( paramcd, aval ) %>%
  filter  ( avisitn == min( avisitn )) %>%
  mutate  ( fane7.bl = fane7, mFARS.bl = mFARS, USS.bl = USS ) %>%
  mutate  ( age_grp = cut(age_bl, c(0,8,12,16,25,40,100), labels = c('<8y', '8-11y', '12-15y', '16-24y', '25-40y', '>40y' ), right = T)) %>%
  filter  ( !is.na(age_grp))

dm. %<>%
  filter( fane7.bl < 5   ) %>%
  filter( mFARS.bl >= 20 ) %>%
  droplevels()

dt <- dt. %>%
  # join to only keep people in dm., then move on.
  inner_join(
    dm. %>% select(study, sjid, age_grp),
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
if (!is.null(age_groups)) {
  dt <- dt %>% filter(age_grp %in% age_groups)
}

# 2. CALCULATE SAMPLE SIZE ----

# Count unique subjects per visit, study, and age group
sample_size_by_age <- dt %>%
  distinct(study, sjid, age_grp, avisitn) %>%
  group_by(study, age_grp, avisitn) %>%
  summarise(n = n(), .groups = 'drop')

# Count unique subjects per visit and study (all ages combined)
sample_size_all <- dt %>%
  distinct(study, sjid, avisitn) %>%
  group_by(study, avisitn) %>%
  summarise(n = n(), .groups = 'drop')

# 3. PLOT GRAPHS ----

# Define colors by study
study_colors <- c(
  "UNIFAI" = "#377EB8"  # blue
  # Add more studies here as needed
)

# Define colors by age group (from the slopes script)
age_colors <- c(
  "<8y"    = "#E41A1C", # red
  "8-11y"  = "#377EB8", # blue
  "12-15y" = "#4DAF4A", # green
  "16-24y" = "#984EA3", # purple
  "25-40y" = "#FF7F00", # orange
  ">40y"   = "#666666"  # dark gray
)

# Plot 1: All ages combined
p1 <- sample_size_all %>%
  ggplot() +
  aes(x = avisitn, y = n, fill = study) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +
  scale_fill_manual(values = study_colors) +
  scale_x_continuous(breaks = unique(sample_size_all$avisitn)) +
  labs(
    x = "Visit (years)",
    y = "Number of Subjects",
    fill = "Study",
    title = "All Ages"
  ) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  expand_limits(y = max(sample_size_all$n) * 1.15)  # Add space for labels

print(p1)

# Plot 2: By age group
p2 <- sample_size_by_age %>%
  ggplot() +
  aes(x = avisitn, y = n, fill = age_grp) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = n, group = age_grp),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  scale_fill_manual(values = age_colors) +
  scale_x_continuous(breaks = unique(sample_size_by_age$avisitn)) +
  labs(
    x = "Visit (years)",
    y = "Number of Subjects",
    fill = "Age Group",
    title = "By Age Group"
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  expand_limits(y = max(sample_size_by_age$n) * 1.15)  # Add space for labels

print(p2)

# 4. SAVE TO POWERPOINT ----

# FUNCTION: Save plot to PowerPoint ----
save_plot_to_ppt <- function(plot_obj = NULL,
                              title = "title",
                              layout = "TTE",
                              i = 1,
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

# Save both plots to PowerPoint (two slides in one file)
# Create output file path
script_path <- rstudioapi::getSourceEditorContext()$path
if (is.null(script_path) || script_path == "") {
  output_file <- .create_output_filepath("Sample Size by Age Group")
} else {
  output_file <- sub("\\.R$", ".pptx", script_path)
}

# Create PowerPoint with two slides
ppt <- read_pptx(.ppt.template.file)

# Slide 1: All ages
ppt <- ppt %>%
  add_slide(layout = "TTE", master = "CR") %>%
  ph_with(
    dml(print(p1, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Sample Size - All Ages",
    location = ph_location_type(type = "title")
  )

# Slide 2: By age group
ppt <- ppt %>%
  add_slide(layout = "TTE", master = "CR") %>%
  ph_with(
    dml(print(p2, newpage = FALSE)),
    location = ph_location_type(type = "body", type_idx = 1)
  ) %>%
  ph_with(
    "Sample Size - By Age Group",
    location = ph_location_type(type = "title")
  )

# Save file
print(ppt, target = output_file)

message("PowerPoint saved to: ", output_file)
