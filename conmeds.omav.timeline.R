
source('.project.settings.R')

omav. <- readRDS('DATA derived/omav.rds')

# patients with unclear dates ---------------------------------------------

start.unkn. <- omav. %>% ungroup %>% 
  filter( is.na( start )) %>% 
  select( sjid )

omav. %>% 
  inner_join(start.unkn.)

withdrawals. <- omav. %>% 
  group_by(sjid) %>% 
  filter( start == max(start )) %>% 
  filter(!is.na(end)) %>% 
  ungroup() %>% select(sjid)

omav. %>% 
  semi_join(withdrawals.) %>% 
  arrange(sjid, start) %>% 
  # print(n=27) %>% 
  mutate( days = 1+end-start) %>% 
  select( sjid, start, end, days, everything() ) %>% 
  arrange(-days) %>% 
  ungroup %>% mutate( rank = LETTERS[row_number()]) %>% 
  select ( rank, start, days ) %>% .ct

# summary table: type of treatments ------------------------------------------------------

# Summary table showing how many subjects are on each drug type
drug_summary <- omav. %>%
  group_by(type) %>%
  summarise(
    n_subjects = n_distinct(sjid),
    .groups = 'drop'
  ) %>%
  arrange(-n_subjects)

print(drug_summary)

omav. %>%
  filter( !is.na(start) ) %>% 
  # filter(sjid == 78)
  # mutate(sjid == as.numeric(sjid)) %>% 
  # mutate(sjid = factor(sjid, reorder))
  ungroup %>% 
  mutate( end = if_else(is.na(end), as.Date(today()), end) ) %>% 
  ggplot() +
  aes(
    x    = start,
    xend = end,
    y    = reorder( sjid, start, FUN = min )
  ) +
  aes(color = type) + #scale_fill_brewer( palette = 'Set1')+
  geom_segment(aes(yend = reorder(sjid, start, FUN = min)), linewidth = 1.5, alpha = 0.6)+  # Apply alpha for overlaps
  # labs(title = "Patient Drug Timeline (Sorted by First Start Date)", x = "Date", color = "Source") +
  labs(title = "Timeline of Treatment Uptake (Sorted by First Start Date)", x = "Date", color = "Source") +
  # geom_vline(xintercept = c(as.Date('2016-01-01'), as.Date('2020-03-01'), size = 1), linewidth = 1)+
  # geom_vline(xintercept = c(as.Date('2023-02-28')), color = 'blue', linewidth = 1)+
  .leg('tl')+
  theme(axis.text.y = element_blank())+
  scale_y_discrete(name = "Patient ID (hidden)")

# .sp()


# Combination summary: subjects with multiple treatment types
combination_summary <- omav. %>%
  group_by(sjid) %>%
  summarise(
    n_trts = n_distinct(type),
    treatments = toString(na.omit(unique(type)))
  ) %>%
  select( sjid, treatments ) %>%
  left_join(
    .dd('demo.l') %>% 
      select(study, sjid, site)
    ) %>% 
  group_by(site, treatments) %>%
  summarise(n = n()) %>%
  arrange(-n)

print(combination_summary) %>% 
  spread(treatments, n) %>% 
  arrange(-skyclarys) %>% 
  .p


