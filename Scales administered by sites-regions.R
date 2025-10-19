# 
# 
# # sites with mFARS 
# 
# n is still negligible overall. 
# but cross-sectional it shows. 
# 
# region            n
# 1 North America  6295
# 2 Oceania        1113
# 3 Europe          258
# 4 Asia            219
# 
# EU only:
#   
#   site             n
# 1 UK Aachen       68
# 2 Besta           59
# 3 Motol           51
# 4 Tallaght        34
# 5 MU Innsbruck    15
# 6 Medea           14
# 7 UK Tübingen     11
# 8 Bambino Gesu     3
# 9 Radboud          3
# 


dt. <- bind_rows(
  .dd('fars') %>% filter(study %in% c('CRCSCA','UNIFAI')),
  .dd('sara') %>% filter(study %in% c('CRCSCA','UNIFAI')),
  .dd('adl')  %>% filter(study %in% c('CRCSCA','UNIFAI')) 
) %>% 
  # filter( study == 'UNIFAI' ) %>% 
  filter( paramcd %in% c('SARA','USS','ADL') ) %>% 
  select( -avisit)

dm. <- bind_rows(
  .dd('demo.l')   %>% select( study, sjid, site ),
  .dd('demo.sca') %>% select( study, sjid, site )
  )

tab.UF <- dt. %>% 
  # filter( adt >= '2024-03-01' | study == 'UNIFAI') %>%
  select( -adt ) %>% 
  spread( paramcd, aval ) %>% 
  filter( !is.na( USS ) & !is.na( SARA )) %>% 
  group_by(sjid) %>% 
  filter(avisitn == min ( avisitn ) ) %>% 
  left_join(dm.) %>% 
  group_by(study, site) %>% 
  tally %>% 
  arrange(study, -n)

tab.UF %>% .ct



