
dt.bg <- .rt('../../Ataxia/DATA other/PMABG_USS.txt')

dt.bg.long <- dt.bg %>%
  # filter(param == 'USS') %>% 
  mutate(across(-c(type, param), as.numeric)) %>% 
  pivot_longer(
    cols = -c(time, type, param),
    names_to = "var",
    values_to = "val"
  ) %>%
  separate(var, into = c("analysis","match", "group"), sep = "\\.") %>%
  pivot_wider(
    names_from = type,
    values_from = val
  ) %>%
  # make sure numeric columns are numeric
  mutate(
    change = as.numeric(change),
    n      = as.numeric(n)
  )

dt.bg.long %<>% 
  select(-n) %>% 
  spread( param, change ) %>% 
  mutate( fars.app = mFARS-USS) %>% 
  gather( paramcd, change, mFARS, USS, fars.app )

dt.bg.long %<>% 
  mutate( match = factor( match, c('pooled','oo','po')))

# dt.bg.long %<>% 
#   group_by(analysis, match, group) %>% 
#   mutate(bl.size = max(n, na.rm=T)) %>% 
#   mutate(pct = round(n/bl.size, 2))

# dt.bg.long %>% 
#   ggplot()+  
#   # geom_point(  aes(size = n))+
#   geom_point()+
#   geom_line()+
#   aes(x = time, y = change, color=group)+
#   aes(shape = analysis)+
#   # geom_text(aes(label = pct,y = change-.3))+
#   facet_wrap(~match)+
#   geom_hline(yintercept = 2.4)+
#   theme_minimal()

# dt.bg.long %>% 
#   ggplot()+  geom_point( )+geom_line()+
#   aes(x = time, y = pct, color=group)+
#   aes(shape = analysis)+
#   # .ssm4+
#   facet_wrap(~match)+
#   theme_minimal()

dt.bg.long %>% 
  ggplot()+  
  geom_point(size = 2.5)+
  geom_line(size = .75)+
  aes(x = time, y = change, color=group)+
  aes(shape = analysis)+#.ssmA+
  scale_color_manual(values = c('#3182bd','#de2d26','black'))+
  facet_grid(paramcd~match)+
  geom_hline(yintercept = c(0, 2.4))+
  theme_minimal()+
  .leg('top')

# remove 1st year ---------------------------------------------------------

tm1 <- dt.bg.long %>% 
  filter(time == 1) %>% 
  mutate(y1.change = change) %>% 
  select(analysis, match, group, y1.change)

tm1 <- dt.bg.long %>% 
  left_join(tm1) %>% 
  mutate(change = change - y1.change)

tm1 %>% 
  ggplot()+  
  geom_point(size = 2.5)+
  geom_line(size = .75)+
  aes(x = time, y = change, color=group)+
  aes(shape = analysis)+.ssmA+
  aes(group = paste(analysis, match, group))+
  scale_color_manual(values = c('#3182bd','#de2d26','black'))+
  facet_wrap(~match)+
  geom_hline(yintercept = 2.4)+
  theme_minimal()+
  .leg('top')

dt.bg.long %>% 
  filter(group == 'facoms', time == 1)



# .sp()
# .sp(l = '1s', i = 2)
