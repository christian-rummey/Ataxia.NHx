
dt. <- bind_rows(
  .dd('sara'),
  .dd('fars')
) %>% 
  filter( !avisitn %in% c( 0.5, 1.5 )) %>% 
  select(-avisit) %>% 
  filter( !study %in% c('EFACTS','TRACKFA','FACOMS') ) %>% 
  filter(paramcd %in% c('USS','mFARS','SARA','fSARA')) %>% 
  filter(!paramcd %in% c('mFARS')) %>% 
  group_by(study, sjid, paramcd) %>% 
  filter( avisitn < 3 ) %>% 
  filter(n() > 2) %>% 
  arrange(adt, .by_group = TRUE) %>%
  
  mutate(
    bl      = aval[match(0, avisitn)],        # baseline (NA if none)
    y1      = aval[match(1, avisitn)],        # year-1 value (NA if none)
    cbl     = aval - bl,                      # change vs baseline at each visit
    cbl.1y  = y1 - bl,                        # 1-year change, repeated on all rows
    grp_1y  = case_when(
      is.na(cbl.1y)        ~ NA_character_,   # no 1y data
      cbl.1y > 0           ~ "worsened",
      cbl.1y < 0           ~ "improved",
      TRUE                 ~ "stable"
    )
  ) %>%
  ungroup()


dt. %>% 
  filter( !is.na(grp_1y)) %>% 
  # filter( paramcd == 'SARA' ) %>% 
  group_by(study, paramcd, avisitn, grp_1y ) %>%
  summarise(
    n = n(),
    change.bl = mean(cbl, na.rm=T)
  ) %>% 
  ggplot()+geom_line()+geom_point( aes(size=n))+
  aes(x = avisitn, y = change.bl)+
  aes(color = grp_1y)+
  facet_grid(study ~ paramcd )
