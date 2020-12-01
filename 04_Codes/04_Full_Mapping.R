# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ Eagle potential
# Purpose:      Potential division
# programmer:   Zhe Liu
# Date:         2020-11-27
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Internal & potential data ----
## internal
internal.std <- read.xlsx('03_Outputs/Internal_Standard_20201127.xlsx')

## potential
eagle.potential.fmt <- read_excel('03_Outputs/Eagle_Potential_Format.xlsx')


##---- Full mapping ----
## universe
eagle.universe <- eagle.potential.fmt %>% 
  mutate(STANDARD_NAME = stri_paste(Prefecture, `机构名称`)) %>% 
  left_join(internal.std[internal.std$flag %in% c('IT', 'Manual'), ], by = 'STANDARD_NAME') %>% 
  select(-Province_I, -City_I, -Prefecture_I)

## prefecture
eagle.pft <- eagle.universe %>% 
  filter(is.na(flag)) %>% 
  select(-NAME, -CV, -DM, -RE, -flag) %>% 
  full_join(internal.std[internal.std$flag %in% c('Unmatched'), ], 
            by = c('Province' = 'Province_I', 'City' = 'City_I', 'Prefecture' = 'Prefecture_I', 'STANDARD_NAME')) %>% 
  group_by(Province, City, Prefecture) %>% 
  mutate(`潜力合计` = first(`潜力合计`), 
         `内部销量合计` = first(`内部销量合计`), 
         `潜力新分组` = first(`潜力新分组`), 
         `内部销量新分组` = first(`内部销量新分组`), 
         `Segment-New` = first(`Segment-New`), 
         `MS%` = first(`MS%`), 
         `份额分组` = first(`份额分组`), 
         Decile = first(Decile), 
         `潜力分组` = first(`潜力分组`), 
         `份额高低` = first(`份额高低`), 
         `区县分类` = first(`区县分类`), 
         CV1 = first(CV1) + sum(CV[is.na(CV1)]), 
         DM1 = first(DM1) + sum(DM[is.na(DM1)]), 
         RE1 = first(RE1) + sum(RE[is.na(RE1)])) %>% 
  ungroup()

## potential check
chk.ptt <- eagle.full %>% 
  group_by(Province, City, Prefecture) %>% 
  summarise(CV1 = first(na.omit(CV1)), 
            DM1 = first(na.omit(DM1)), 
            RE1 = first(na.omit(RE1)), 
            CV = sum(CV, na.rm = TRUE), 
            DM = sum(DM, na.rm = TRUE), 
            RE = sum(RE, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(cvflag = CV1 < CV, 
         dmflag = DM1 < DM, 
         reflag = RE1 < RE) %>% 
  filter(cvflag == 1 | dmflag == 1 | reflag == 1) %>% 
  mutate(cvdiff = (CV - CV1) / CV1, 
         dmdiff = (DM - DM1) / DM1, 
         rediff = (RE - RE1) / RE1)

## full
eagle.full <- eagle.universe %>% 
  filter(!is.na(flag)) %>% 
  bind_rows(eagle.pft) %>% 
  group_by(Province, City, Prefecture) %>% 
  mutate(CV1 = last(na.omit(CV1)), 
         DM1 = last(na.omit(DM1)), 
         RE1 = last(na.omit(RE1)), 
         patients = if_else(is.na(patients), quantile(na.omit(patients), 0.1), patients)) %>% 
  ungroup() %>% 
  group_by(Province, City) %>% 
  mutate(CV1 = if_else(is.na(CV1), quantile(na.omit(CV1), 0.1), CV1), 
         DM1 = if_else(is.na(DM1), quantile(na.omit(DM1), 0.1), DM1), 
         RE1 = if_else(is.na(RE1), quantile(na.omit(RE1), 0.1), RE1), 
         patients = if_else(is.na(patients), quantile(na.omit(patients), 0.1), patients)) %>% 
  ungroup() %>% 
  group_by(Province) %>% 
  mutate(CV1 = if_else(is.na(CV1), quantile(na.omit(CV1), 0.1), CV1), 
         DM1 = if_else(is.na(DM1), quantile(na.omit(DM1), 0.1), DM1), 
         RE1 = if_else(is.na(RE1), quantile(na.omit(RE1), 0.1), RE1), 
         patients = if_else(is.na(patients), quantile(na.omit(patients), 0.1), patients)) %>% 
  ungroup() %>% 
  group_by(Province, City, Prefecture) %>% 
  mutate(CV_margin = CV1 - sum(CV, na.rm = TRUE), 
         DM_margin = DM1 - sum(DM, na.rm = TRUE), 
         RE_margin = RE1 - sum(RE, na.rm = TRUE), 
         ratio = patients / sum(patients)) %>% 
  ungroup() %>% 
  mutate(CV2 = CV_margin * ratio + if_else(is.na(CV), 0, CV), 
         DM2 = DM_margin * ratio + if_else(is.na(DM), 0, DM), 
         RE2 = RE_margin * ratio + if_else(is.na(RE), 0, RE)) %>% 
  rename(CV_pft = CV1, 
         DM_pft = DM1, 
         RE_pft = RE1, 
         CV_hc = CV2, 
         DM_hc = DM2, 
         RE_hc = RE2) %>% 
  select(-patients, -ratio, -STANDARD_NAME, -CV_margin, -DM_margin, -RE_margin)

write.xlsx(eagle.full, '03_Outputs/Eagle_Potential_Internal.xlsx')


