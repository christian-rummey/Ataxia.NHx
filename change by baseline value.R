

source('.project.settings.R')

pars. <- c('SARA','mFARS','USS','FARS.B')
studies <- c('UNIFAI')

dt. <- bind_rows(
  .dd('sara'),
  .dd('fars'),
  .dd('adl')
) %>% 
  # filter(paramcd %in% pars.) %>% 
  filter(study %in% studies) %>% 
  filter(paramcd == 'SARA.ap')


dt <- dt. %>% 
  group_by( study, sjid ) %>% 
  arrange( study, sjid, paramcd, tm. ) %>% 
  mutate( chg = lead( aval ) - aval  ) %>% 
  mutate( int = lead(tm.) - tm. ) %>% 
  mutate( bl.grp = cut( aval, seq(0, 94,  2), include.lowest = T )) %>% 
  filter ( int >0.5 & int < 1.5 ) %>% 
  filter(!is.na(chg))

dt   %>%
  group_by ( study, paramcd, bl.grp ) %>% 
  summarise( m = mean (chg), sd = .ci (chg)) %>% 
  ggplot()+geom_col()+
  aes(x = bl.grp, y = m)+
  geom_errorbar(aes(ymin = m-sd, ymax = m+sd, width = .1))
    



