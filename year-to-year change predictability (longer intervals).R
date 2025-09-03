library(dplyr)
library(purrr)
library(tidyr)

# assumes dt. has: study, sjid, paramcd, adt, aval
# 1) build annualized changes and visit index per subject
dd <- dt. %>%
  arrange(study, sjid, paramcd, adt) %>%
  group_by(study, sjid, paramcd) %>%
  mutate(
    int_b = as.numeric(adt - lag(adt))/365.25,
    int_a = as.numeric(lead(adt) - adt)/365.25,
    chg_b = (aval - lag(aval))/int_b,     # prior-year annualized change
    chg_a = (lead(aval) - aval)/int_a,    # next-year annualized change
    t_idx = row_number()
  ) %>%
  ungroup()

# 2) function to compute future k-year slope starting at each index
future_k <- function(df, k){
  df %>%
    group_by(study, sjid, paramcd) %>%
    arrange(adt, .by_group = TRUE) %>%
    mutate(
      # cumulative future change over k years (allow ragged intervals via rolling endpoint)
      adt_k  = adt + k*365.25,
      # find the first time >= adt_k and linearly interpolate aval_k between surrounding visits
      aval_next = lead(aval),
      adt_next  = lead(adt),
      rate_next = (aval_next - aval) / as.numeric(adt_next - adt),     # per-day rate to interpolate
      aval_k_approx = aval + rate_next * pmax(0, pmin(adt_k - adt, as.numeric(adt_next - adt))),
      ok = adt_next >= adt_k,  # only keep rows where next visit passes the k-year mark
      k_slope = (aval_k_approx - aval) / k                             # annualized k-year slope
    ) %>%
    ungroup() %>%
    filter(ok, is.finite(chg_b), is.finite(k_slope)) %>%
    select(study, paramcd, sjid, t_idx, chg_b, k_slope)
}

res <- bind_rows(
  future_k(dd, 1) %>% mutate(k = 1),
  future_k(dd, 2) %>% mutate(k = 2),
  future_k(dd, 3) %>% mutate(k = 3)
)

# 3) summarize correlations and simple slopes by cohort/scale and horizon
summary_k <- res %>%
  group_by(study, paramcd, k) %>%
  summarise(
    n = n(),
    cor = cor(chg_b, k_slope, use = "complete.obs"),
    slope = coef(lm(k_slope ~ chg_b))[2],
    r2 = summary(lm(k_slope ~ chg_b))$r.squared,
    .groups = "drop"
  ) %>%
  arrange(paramcd, study, k)

summary_k
