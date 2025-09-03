
# data --------------------------------------------------------------------

dt. <- bind_rows(
  .dd('sara'),
  .dd('fars')
  ) %>% 
  filter( !study %in% c('EFACTS','TRACKFA','FACOMS') ) %>% 
  # filter(paramcd %in% c('SARA','fSARA')) %>% 
  filter(paramcd %in% c('USS','mFARS','SARA','fSARA')) %>% 
  group_by(study, sjid, paramcd) %>% 
  filter(n() > 2) %>% 
  arrange(adt, .by_group = TRUE) %>%
  # interval-aware deltas
  mutate(
    int_b = as.numeric(adt - lag(adt)) / 365.25,
    int_a = as.numeric(lead(adt) - adt) / 365.25,
    chg_b = aval - lag(aval),
    chg_a = lead(aval) - aval,
    # annualized changes
    chg_by = chg_b / int_b,
    chg_ay = chg_a / int_a
  ) %>% 
  # need both sides present
  filter(!is.na(int_b), !is.na(int_a), !is.na(chg_by), !is.na(chg_ay)) %>% 
  # (optional) keep ~1-year intervals
  filter(between(int_b, 0.6, 1.4), between(int_a, 0.6, 1.4)) %>% 
  ungroup()

# per-facet stats: slope of chg_ay ~ chg_by, R², and Pearson r -----------------
dt. %>%
  group_by(study, paramcd) %>%
  group_modify(~{
    fit <- lm(chg_ay ~ chg_by, data = .x)
    tibble(
      n     = nrow(.x),
      slope = coef(fit)[["chg_by"]],
      r2    = summary(fit)$r.squared,
      cor   = cor(.x$chg_by, .x$chg_ay, use = "complete.obs"),
      p     = summary(fit)$coefficients["chg_by","Pr(>|t|)"]
    )
  }) %>%
  ungroup() %>%
  arrange(paramcd, study)

# scatter with identity line and lm fit ---------------------------------------

dt. %>% 
  filter( abs(chg_b) < 10, abs(chg_a) < 10) %>% 
  # filter(avisitn == 1) %>% 
  ggplot(aes(x = chg_by, y = chg_ay)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(study ~ paramcd, scales = "free") +
  # facet_grid(paramcd ~ study, scales = "free") +
  # facet_wrap(study ~ paramcd, scales = "fixed") +
  labs(x = "Annualized change in prior year",
       y = "Annualized change in subsequent year",
       title = "Prior-year change poorly predicts next-year change") +
  # stat_regline_equation()+
  stat_cor(aes(label = after_stat(rr.label)), r.digits = 2)+
  # coord_equal() +                      # keeps slope comparability to y=x
  theme_minimal()

