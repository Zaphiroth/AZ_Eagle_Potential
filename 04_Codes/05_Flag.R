# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ Eagle potential
# Purpose:      Potential division
# programmer:   Zhe Liu
# Date:         2020-12-08
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


## universe
eagle.universe <- eagle.potential.fmt %>% 
  # mutate(STANDARD_NAME = stri_paste(Prefecture, `机构名称`)) %>% 
  left_join(internal.std[internal.std$flag %in% c('IT', 'Manual'), ], by = 'STANDARD_NAME') %>% 
  select(-Province_I, -City_I, -Prefecture_I)

# chk <- eagle.universe %>% 
#   group_by(Province, City, Prefecture, flag_U) %>% 
#   summarise(n = length(na.omit(NAME))) %>% 
#   ungroup() %>% 
#   mutate(flag_U = if_else(is.na(flag_U), 0, flag_U)) %>% 
#   pivot_wider(id_cols = c(Province, City, Prefecture), 
#               names_from = flag_U, 
#               values_from = n, 
#               values_fill = 0)

chk <- eagle.universe %>% 
  group_by(Province, City, Prefecture) %>% 
  summarise(CV1 = first(CV1), 
            DM1 = first(DM1), 
            RE1 = first(RE1), 
            CV = sum(CV, na.rm = TRUE), 
            DM = sum(DM, na.rm = TRUE), 
            RE = sum(RE, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(CV > CV1 | DM > DM1 | RE > RE1)

eagle.universe.ratio <- eagle.universe %>% 
  group_by(Province, City, Prefecture) %>% 
  mutate(CV1 = if_else(first(CV1) < sum(CV, na.rm = TRUE), 
                       first(CV1) + (sum(CV, na.rm = TRUE) - first(CV1)) * 1.1, 
                       first(CV1)), 
         DM1 = if_else(first(DM1) < sum(DM, na.rm = TRUE), 
                       first(DM1) + (sum(DM, na.rm = TRUE) - first(DM1)) * 1.1, 
                       first(DM1)), 
         RE1 = if_else(first(RE1) < sum(RE, na.rm = TRUE), 
                       first(RE1) + (sum(RE, na.rm = TRUE) - first(RE1)) * 1.1, 
                       first(RE1))) %>% 
  ungroup() %>% 
  group_by(Province, City, Prefecture) %>% 
  mutate(CV_margin = CV1 - sum(CV, na.rm = TRUE), 
         DM_margin = DM1 - sum(DM, na.rm = TRUE), 
         RE_margin = RE1 - sum(RE, na.rm = TRUE), 
         ratio = patients / sum(patients)) %>% 
  ungroup() %>% 
  mutate(ratio = if_else(is.na(ratio), 1, ratio), 
         CV_hc = CV_margin * ratio + if_else(is.na(CV), 0, CV), 
         DM_hc = DM_margin * ratio + if_else(is.na(DM), 0, DM), 
         RE_hc = RE_margin * ratio + if_else(is.na(RE), 0, RE)) %>% 
  select(-patients, -ratio, -CV_margin, -DM_margin, -RE_margin, -CV1, -DM1, -RE1)

## bind
eagle.mod <- read.xlsx('02_Inputs/Eagle_Potential_Internal_Mod_20201204.xlsx')

eagle.full.joint <- eagle.mod %>% 
  filter(!is.na(NAME), 
         !(`TM编码` %in% eagle.universe.ratio$`TM编码`), 
         !(NAME %in% eagle.universe.ratio$NAME)) %>% 
  bind_rows(eagle.universe.ratio) %>% 
  select(Province, City, Prefecture, `潜力合计`, `内部销量合计`, `潜力新分组`, `内部销量新分组`, 
         `Segment-New`, `MS%`, `份额分组`, Decile, `潜力分组`, `份额高低`, `区县分类`, 
         CV_ptt = CV_hc, DM_ptt = DM_hc, RE_ptt = RE_hc, `机构名称`, `TM编码`, `地址`, `邮编`, 
         NAME, CV, DM, RE) %>% 
  arrange(Province, City, Prefecture, `机构名称`)

write.xlsx(eagle.full.joint, '03_Outputs/Eagle_Potential_Internal_Joint.xlsx')

chk <- eagle.full.joint %>% 
  filter(!is.na(机构名称)) %>% 
  add_count(Province, City, Prefecture, 机构名称) %>% 
  filter(n > 1)


