#' ---
#' title: "DM.add.slope.changes.R"
#' author: "Christian Rummey"
#' date: "`r format(Sys.time(), '%d %B, %Y, %H:%M')`"
#' output:
#'    html_document:
#'      toc: true
#'      code_folding: show
#'      number_sections: no
#'      toc_float: no
#'      highlight: tango
#' ---

# Redesigned Oct 21
# should work in "update" mode: 
# only datasets (parameters) uncommented should be worked on
# default (git) state should be only few or all uncommented
# does currently not work for FACHILD (only 1y, 2y intervals)

# . -----------------------------------------------------------------------

rm(list = ls())

# get pars and ds.list ----------------------------------------------------

source('../DATA other/parlists.R') 

# compile datasets --------------------------------------------------------

for (ds in ds.list) {
  
  dt. <- data.frame() %>% as_tibble()
  
  dt. %<>% 
    bind_rows(
      .dd( ds ) %>% filter( paramcd %in% pars )
    ) %>% 
    filter( study %in% studies )
  
  table(dt.$study, dt.$paramcd)

# remove patients without symp --------------------------------------------
# principle: have only patients with symp, 
# and that have at least one measurement
  sjids.keep <- bind_rows(
    .dd.atx('demo')     %>% filter(!is.na(sev.o)),
    .dd.atx('demo.sca') %>% filter()
    ) %>% 
    select(sjid) %>% deframe

  dt. %<>% 
    filter(sjid %in% sjids.keep)

  rm(sjids.keep)

  # remove undable from timedM and add unable-pars --------------------------

  dt. %<>% 
    mutate(paramcd = factor(paramcd, pars))
  
  # dt. %>% 
  #   filter(is.na(adt))
  
  steps. <- bind_rows(
    .dd.atx('steps'),
    .dd.atx('steps.sca')
  ) %>% 
    select( study, sjid, avisitn, amb, neuro.score )
  
  dt. %>% 
    left_join( steps. ) %>%
    .add.time( tm = 'age', keepadt = T) %>% 
    filter ( !is.na(amb) ) %>% 
    ungroup
  
  rm(steps.)

  # remove non-ambulatory t25fw ------------------------------------------------
  # this is just for evaluation purposes - I don't use these values currently
  
  dt. %<>% 
    filter (!(paramcd == 'w25.i' & amb == 'non-amb.')) %>% 
    filter (!(paramcd == 'w25.iu' & amb == 'non-amb.')) 
  
  # remove empty avals (can they be worth anything?) ---------------------------

  dt. %>% 
    filter(is.na(aval)) %>% 
    droplevels() %>% select(paramcd) %>% table

  dt. %<>% 
    filter(!is.na(aval))

  # save baseline by amb; only for covariate -----------------------------------
  
  base <- dt. %>%
    group_by( study, sjid, paramcd, amb) %>% # baseline by-amb!!
    filter  ( avisitn == min ( avisitn ) ) %>%
    rename  ( bl.age = age, bl = aval, ) %>%
    select  ( study, sjid, paramcd, bl.age, bl) %>% 
    ungroup

  # calculate changes and intervals for 4 following visits ---------------------
  
  chg.all <- dt. %>%
    # filter(sjid == 4181, avisitn %in% c(1,2)) %>% 
    group_by( study, sjid, paramcd, amb) %>% 
    arrange ( study, sjid, paramcd, avisitn, amb ) %>%
    mutate  ( 
      int_s1   = lead(age    ) - age, 
      int_s2   = lead(age, 2 ) - age, 
      int_s3   = lead(age, 3 ) - age, 
      int_s4   = lead(age, 4 ) - age, 
      int_s5   = lead(age, 5 ) - age, 
      chg_s1   = lead(aval    ) - aval, 
      chg_s2   = lead(aval, 2 ) - aval, 
      chg_s3   = lead(aval, 3 ) - aval, 
      chg_s4   = lead(aval, 4 ) - aval, 
      chg_s5   = lead(aval, 5 ) - aval
      ) %>% 
    pivot_longer(
      cols = int_s1:chg_s5,
      names_to = c("par",'step'),
      names_pattern = "(.*)_(.*)",
      values_to = "value"
    ) %>%
    spread ( par, value )

  chg.all %<>% 
    mutate ( r.int = round(int) ) %>% 
    group_by (avisitn, r.int, .add = T) %>% 
    mutate ( dev.y = abs( int - r.int )) %>% 
    filter ( dev.y == min(dev.y) | is.na(dev.y)) %>% 
    filter ( r.int %in% c(1,2)) %>% 
    mutate ( int = paste(r.int, 'y', sep = '')) %>% 
    ungroup %>% 
    mutate_at('dev.y', round, 3) %>% 
    select( study, sjid, paramcd, avisitn, int, chg, dev.y)

  # add changes to dt -------------------------------------------------------

  dt.all <- chg.all %>%
    right_join( dt. %>% ungroup  ) %>%
    left_join ( base ) %>% 
    select( names(dt.), everything() ) %>% 
    group_by( study, sjid, paramcd, avisitn ) %>% 
    mutate( forslope = rank(int, ties.method = 'first')) %>% 
    mutate( forslope = ifelse(forslope>1, NA, forslope)) %>% 
    group_by( study, sjid, paramcd, amb)

  dt.all %>% 
    .wds( paste('../DATA derived/', ds, '.slope.chg' , sep = '') )
  
  with(dt.all, table(paramcd, forslope, exclude = F))
  with(dt.all, table(paramcd, amb, forslope, exclude = F))
  with(dt.all, table(paramcd, int, exclude = F))

}

