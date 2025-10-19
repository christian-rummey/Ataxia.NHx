
rm(list = ls())

omav.string <- 'clary|claris|omave|loxo|clayr|carys|RTA-408|compassion|moxie'

# Screening Started Oct, 2017
# Screening Ended Nov 05th, 2018
# LPLV must be around Oct 2019
# REATA press Release Oct 14, 2019
# FDA approval 

# UNIFAI ------------------------------------------------------------------

omav.unifai <- .ds.UNIFAI('cm') %>% 
  filter( grepl(omav.string, cmtrt, ignore.case = T)) %>% 
  select( sjid, cmtrt, cmstdat, cmstdat_p, cmstdat_coded, cmendat, cmendat_p, cmendat_coded, cmongo, cmongo_coded )

omav.unifai %<>% 
  .proc_unifai_dts(cmstdat, cmendat) %>% 
  select(sjid, cmtrt, cmstdat, cmendat) %>% 
  # mutate(
  #   cmendat = case_when(
  #     cmendat == cmstdat ~ cmendat + days(0), # Add 3 days if same as start date
  #     TRUE ~ cmendat # Otherwise, keep as is
  #   )
  # ) %>% 
  select(sjid, start = cmstdat, end = cmendat, trt = cmtrt ) %>% 
  mutate( trt = tolower(trt)) %>% 
  mutate( sjid = as.character(sjid)) %>% 
  mutate( data.source = 'unifai.conmeds' )

# FACOMS/UNIFAI conmeds data that was overridden when Medrio added --------
# new comneds. Jan 2025-01

omav.overriden <- read_excel('../../Medrio/2025-01-16 prepost upload comparison/MedrioExport202412102226_harish_conmeds/CM.xlsx')

names(omav.overriden) <- tolower(names(omav.overriden))

omav.overriden %<>% 
  filter( grepl(omav.string, cmtrt, ignore.case = T) ) %>%
  select( sjid = medrioid, cmtrt, cmstdat, cmstdat_p, cmendat, cmendat_p ) %>% 
  mutate( sjid = as.character( sjid ) ) %>% 
  .proc_unifai_dts( cmstdat, cmendat ) %>% 
  select( sjid, trt = cmtrt, start = cmstdat, end = cmendat ) %>% 
  mutate( trt = tolower(trt)) %>% 
  mutate( data.source = 'omav.overriden' )

omav. <- bind_rows(
  omav.unifai,
  omav.overriden %>% 
    filter(!sjid %in% c(4684, 4870))
) %>% 
  arrange(sjid) %>% 
  group_by( sjid, start, end, trt ) %>% 
  # filter(n()>1) %>% 
  summarise_all( ~toString(na.omit(.)) ) %>% 
  ungroup

# dups

omav. %<>% 
  filter( !(sjid == 4360 & start == as.Date('2019-11-08')))

omav. %<>% 
  mutate( type = ifelse( start >= as.Date ('2023-03-01'), 'skyclarys'      , 'omav' )) %>% 
  mutate( type = ifelse( grepl('plac', trt)             , 'omav or placebo', type)) %>% 
  mutate( type = ifelse( start <= as.Date ('2018-09-01'), 'omav or placebo', type ))

# seems appropriate
omav. %<>% 
  mutate(type = ifelse(is.na(type), 'skyclarys', type))

# # these look ok
# omav. %>% 
#   group_by(sjid) %>% filter(n()==1) %>% 
#   arrange(sjid) %>% 
#   arrange(start) %>% 
#   .ct

# missing dates -----------------------------------------------------------

missing.start. <- omav. %>% 
  filter(is.na(start)) %>% 
  select(sjid)

omav. %>% 
  right_join(missing.start.)

# all facoms are in omav.already.  ----------------------------------------

# omav.facoms.comneds <- .rd.FACOMS('conmed') %>% 
#   filter( grepl(omav.string, cmtrt2, ignore.case = T) ) %>%
#   select( sjid, site, cmtrt2, startdtb, stopdtb )
# 
# omav.facoms.comneds %<>% 
#   mutate ( 
#     trt    = cmtrt2,
#     start  = startdtb,
#     end    = stopdtb,
#     source = 'facoms.conmeds'
#   ) %>% 
#   select( sjid, start, end, trt, source )
# 
# omav.facoms.comneds %>% 
#   filter(sjid %in% omav.$sjid)
# 
# famed -------------------------------------------------------------------

omav.famed <- .ds.FACOMS('famed') %>% 
  filter( grepl(omav.string, cmtrt, ignore.case = T)) %>% 
  filter( !grepl('EPI-743', cmtrt, ignore.case = T)) %>% 
  select( study, sjid, avisitn, adt, cmtrt, cmtrtyn)

omav.famed %>% 
  group_by(sjid) %>% 
  filter( n()>1 ) %>% 
  print(n=33)

omav.famed %<>% 
  mutate( end = NA, data.source = 'famed' ) %>% 
  mutate( cmtrt = tolower(cmtrt)) %>% 
  select( sjid, start = adt, end, trt = cmtrt, data.source )

omav.famed %<>% 
  group_by(sjid) %>% arrange(start) %>% slice(1) %>% 
  mutate( type = ifelse( start >= as.Date ('2023-03-01'), 'skyclaris'      , 'omav' )) %>% 
  mutate( type = ifelse( grepl('plac', trt)             , 'omav or placebo', type)) %>% 
  mutate( type = ifelse( start <= as.Date ('2018-09-01'), 'omav or placebo', type ))

# have new info in famed. -------------------------------------------------

omav.famed %>%
  filter((sjid %in% omav.$sjid)) %>% 
  print(n=51)

# these subjects have additional info in famed. 
# manually checked vs omav. 2025-03-01
# many of the famed dates are later than in omav. - ignored

famed.new.info <- c( 50, 225, 4219, 4455, 4777, 4946 )

# add those that are just new ---------------------------------------------

omav. %<>% 
  bind_rows(
    omav.famed %>% 
      filter(!(sjid %in% omav.$sjid))
  ) %>% 
  group_by(sjid)

# check pot. new infos ---------------------------------------------------

omav. %>% 
  bind_rows(
    omav.famed %>% 
      filter((sjid %in% omav.$sjid))
  ) %>% 
  group_by( sjid ) %>% 
  filter  ( sjid %in% famed.new.info ) %>% 
  arrange ( sjid, start )

# execute (manual comparison above)

omav. %<>% 
  filter( !sjid %in% c( 4219, 4455, 4383, 4371 ) ) %>% 
  bind_rows(
    omav.famed %>% 
      filter((sjid %in% omav.$sjid))
  ) %>% 
  group_by(sjid) %>% 
  # filter(sjid %in% famed.new.info) %>% 
  arrange(sjid,start)

rm(omav.famed, omav.overriden, omav.unifai)

# add last visit ----------------------------------------------------------

last.vd. <- .ds.UNIFAI('vf') %>% 
  filter(vfperf == 'Yes') %>% 
  filter(!is.na(adt)) %>%
  # select( -crf ) %>% 
  unique %>%
  group_by( sjid ) %>% 
  filter ( adt == max(adt) ) %>% 
  # unique() %>% filter(n()>1)
  slice(1) %>% 
  select(study, sjid, adt)

omav. %<>% 
  left_join(
    last.vd. %>% 
      rename( last.visit.adt = adt )
  )

# . ----------------------------------------------------------------------

#not sure why not unique! (multiple sources)
omav. %<>% 
  unique() 

# when end unknown, use fu start date -1 ----------------------------------

omav. <- bind_rows(
  
  omav. %>% 
    group_by(sjid) %>%
    filter ( n()>1 ) %>% 
    arrange( sjid, start ) %>% 
    mutate ( end = lead(start)-1 )
  ,
  omav. %>% 
    group_by(sjid) %>%
    filter ( n() == 1 )
)  

# add withdrawals ---------------------------------------------------------
# there are 4 - 2025-10

omav. %<>% 
  left_join(
    .ds.UNIFAI('eos') %>% 
      filter(sjid %in% unique(omav.$sjid)) %>% 
      select(study, sjid, eosrea)
    )

# write -------------------------------------------------------------------

omav. %>% 
  saveRDS('DATA derived/omav.rds')
