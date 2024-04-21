#' ---
#' title: "DM.data.cleaner.R"
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

#+ setup, include=FALSE
knitr::opts_chunk$set(collapse = T)
knitr::opts_chunk$set(echo     = T)
knitr::opts_chunk$set(include  = T)
knitr::opts_chunk$set(warning  = F)

#' ### Data 

# packages ----------------------------------------------------------------
rm(list = ls())

# data --------------------------------------------------------------------

source('../DATA other/parlists.R')

dt. <- data.frame() %>% as_tibble()

for (ds in ds.list) {
  dt. %<>% 
    bind_rows(
      .dd.FA( paste('long.data', ds, sep= '.' ) ) %>% filter(paramcd %in% pars)
    )
}
table(dt.$study, dt.$paramcd)

dt. %<>% 
  filter ( study %in% studies ) %>% 
  filter ( paramcd %in% pars )

# . -----------------------------------------------------------------------

lmer.nh <- dt. %>% 
  .add.demo() %>% 
  mutate( age.grp = cut(age, agegrps, labels = agegrps.l, right = T)) %>% 
  droplevels()

lmer.nh.clean <- lmer.nh %>% 
  filter( pm == 0 ) %>% 
  filter( !is.na(gaa1) ) %>% 
  filter( !is.na(symp) ) %>% 
  mutate( gaa1 = gaa1/100) %>% 
  arrange(paramcd, amb, sev.o) %>% 
  filter(!is.na(age.grp)) %>%
  droplevels()

levels( lmer.nh.clean$age.grp )
range ( lmer.nh.clean$age )
with  ( lmer.nh.clean, table(paramcd, study))
with  ( lmer.nh.clean, table(int, forslope, exclude = F))

# write -------------------------------------------------------------------

lmer.nh %>% 
  saveRDS( paste( 'DATA derived/long.data', ds ,'rds', sep = '.') )

lmer.nh.clean %>% 
  saveRDS( paste( 'DATA derived/long.data', ds ,'clean.rds', sep = '.'))

# this is the only thing that needs to be done from the clean lmer --------

# pars <- levels(readRDS('DATA derived/lmer.data.rds')$paramcd)
# pars <- c('mFARS','FARS.E')
# 
# dt. %<>% 
#   filter( amb         == 'ambulatory') %>% 
#   filter( neuro.score >= 20)
# 
# dt. %<>%
#   select ( study, sjid, symp, gaa1, sev.o, 
#            age.grp, avisitn, age, time., amb, neuro.score, 
#            paramcd, bl, aval, chg, 
#            int, dev.y, forslope
#            )