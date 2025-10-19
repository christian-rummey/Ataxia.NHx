
# intro -------------------------------------------------------------------

source('.settings.R')
library(broom.mixed)
library(purrr)
library(lme4)

# pars. <- c('SARA','mFARS','USS','FARS.B')
pars. <- c('SARA.ap','SARA.ax','SARA')
pars. <- c('SARA.ap','SARA.ax','USS','FARS.B')
studies <- c('UNIFAI')

dt. <- bind_rows(
  .dd('sara'),
  .dd('fars'),
  .dd('adl')
) %>%
  # filter(paramcd %in% c(.l.sara,'SARA'))
  filter(paramcd %in% pars.) %>%
  filter(study %in% studies)

# dt. %>% 
#   filter( paramcd %in% .l.sara.ap ) %>% 
#   filter( aval > 4 ) %>% 
#   spread( paramcd, aval ) %>% .p
#   spread( paramcd, aval ) %>% 
#   filter( s8.hesh >4 )
#   filter( SARA > 40 ) %>% arrange(sjid) 
#   filter(paramcd == 'SARA') %>% filter(aval>40)

# work --------------------------------------------------------------------

# Nest data by study and paramcd
nested_data <- dt. %>%
  group_by(study, paramcd) %>%
  nest() %>%
  ungroup()


# Calculate individual subject slopes
subject_slopes <- nested_data %>%
  mutate(
    slopes = map(data, function(df) {
      df %>%
        group_by(sjid) %>%
        arrange(sjid, tm.) %>%
        filter(!is.na(aval), !is.na(tm.)) %>%
        filter(n() >= 2) %>%
        do({
          baseline_val <- first(.$aval)
          model <- lm(aval ~ tm., data = .)
          slope_val <- coef(model)[['tm.']]

          tibble(
            baseline = baseline_val,
            slope = slope_val,
            n_obs = nrow(.),
            time_span = max(.$tm.) - min(.$tm.)
          )
        }) %>%
        ungroup() %>%
        mutate(bl.grp = cut(baseline, seq(0, 94, 4), include.lowest = TRUE))
    })
  ) %>%
  select(study, paramcd, slopes) %>%
  unnest(slopes)


# Step 1: Prepare data with baseline groups
nested_with_bl <- nested_data %>%
  mutate(
    model_data = map(data, function(df) {
      df %>%
        group_by(sjid) %>%
        mutate(baseline = first(aval)) %>%
        ungroup() %>%
        mutate(bl.grp = cut(baseline, seq(0, 94, 4), include.lowest = TRUE)) %>%
        filter(!is.na(bl.grp)) %>%
        filter(n_distinct(sjid) >= 5)  # Need enough subjects
    })
  )

# Step 2: Fit lmer models using map
nested_with_models <- nested_with_bl %>%
  mutate(
    model = map(model_data, possibly(
      function(df) {
        lmer(aval ~ bl.grp * tm. + (1 + tm. | sjid),
             data = df,
             control = lmerControl(optimizer = "bobyqa"))
      },
      otherwise = NULL
    )),
    bl_levels = map(model_data, ~ levels(.x$bl.grp))
  )

# Step 3: Tidy coefficients using map
nested_with_coefs <- nested_with_models %>%
  mutate(
    coefs = map(model, possibly(
      function(mod) {
        tidy(mod, effects = "fixed")
      },
      otherwise = tibble(term = character(), estimate = numeric(), std.error = numeric())
    ))
  )

# Step 4: Extract slopes by baseline group
mixed_slopes <- nested_with_coefs %>%
  select(study, paramcd, coefs, bl_levels) %>%
  mutate(
    slopes = map2(coefs, bl_levels, function(coef_df, bl_lvls) {
      if (nrow(coef_df) == 0 | is.null(bl_lvls)) {
        return(tibble(bl.grp = character(), slope_estimate = numeric(), std.error = numeric()))
      }

      # Get all slope-related terms
      slope_terms <- coef_df %>%
        filter(grepl("tm\\.", term))

      if (nrow(slope_terms) == 0) {
        return(tibble(bl.grp = character(), slope_estimate = numeric(), std.error = numeric()))
      }

      # Baseline slope (reference group)
      baseline_slope <- slope_terms %>%
        filter(term == "tm.") %>%
        pull(estimate)

      if (length(baseline_slope) == 0) baseline_slope <- 0

      # Process all slope terms
      slope_terms %>%
        mutate(
          bl.grp = if_else(
            term == "tm.",
            bl_lvls[1],
            gsub("bl\\.grp(.+):tm\\.", "\\1", term)
          ),
          slope_estimate = if_else(
            term == "tm.",
            estimate,
            estimate + baseline_slope
          )
        ) %>%
        select(bl.grp, slope_estimate, std.error) %>%
        mutate(bl.grp = factor(bl.grp, levels = bl_lvls))
    })
  ) %>%
  select(study, paramcd, slopes) %>%
  unnest(slopes)

# Plot mixed model results
mixed_slopes %>%
  filter(!is.na(slope_estimate)) %>%
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(aes(x = bl.grp, y = slope_estimate), size = 3) +
  geom_errorbar(aes(x = bl.grp,
                    ymin = slope_estimate - 1.96 * std.error,
                    ymax = slope_estimate + 1.96 * std.error,
                    width = 0.2)) +
  facet_wrap(~ study + paramcd, scales = "free") +
  labs(y = "Estimated Slope (points/year)",
       x = "Baseline Value Group",
       title = "Rate of Change by Baseline Value (Mixed Model)",
       subtitle = "Model: aval ~ baseline_group * time + (1 + time | subject)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summary statistics by study, paramcd, and baseline group
summary_table <- subject_slopes %>%
  filter(!is.na(bl.grp)) %>%
  group_by(study, paramcd, bl.grp) %>%
  summarise(
    n_subjects = n(),
    mean_slope = mean(slope, na.rm = TRUE),
    median_slope = median(slope, na.rm = TRUE),
    sd_slope = sd(slope, na.rm = TRUE),
    min_slope = min(slope, na.rm = TRUE),
    max_slope = max(slope, na.rm = TRUE),
    mean_n_obs = mean(n_obs, na.rm = TRUE),
    mean_time_span = mean(time_span, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_table)
