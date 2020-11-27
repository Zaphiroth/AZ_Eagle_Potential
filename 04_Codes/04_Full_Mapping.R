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
  full_join(internal.std[internal.std$flag %in% c('Unmatched'), c(1:4, 6:9)], 
            by = c('Province' = 'Province_I', 'City' = 'City_I', 'Prefecture' = 'Prefecture_I'))

## potential check
chk.ptt <- eagle.universe %>% 
  filter(!is.na(flag)) %>% 
  bind_rows(eagle.pft) %>% 
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
  filter(isTRUE(cvflag) | isTRUE(dmflag) | isTRUE(reflag))

## full
eagle.full <- eagle.universe %>% 
  filter(!is.na(flag)) %>% 
  bind_rows(eagle.pft) %>% 
  group_by(Province, City, Prefecture) %>% 
  mutate(CV1 = first(na.omit(CV1)), 
         DM1 = first(na.omit(DM1)), 
         RE1 = first(na.omit(RE1)), 
         CV_margin = CV1 - sum(CV, na.rm = TRUE), 
         DM_margin = DM1 - sum(DM, na.rm = TRUE), 
         RE_margin = RE1 - sum(RE, na.rm = TRUE), 
         patients = if_else(is.na(patients), quantile(na.omit(patients), 0.75), patients), 
         ratio = patients / sum(patients), 
         ratio = if_else(is.na(ratio), 1/n(), ratio)) %>% 
  ungroup() %>% 
  mutate(CV1 = CV_margin * ratio + CV, 
         DM1 = DM_margin * ratio + DM, 
         RE1 = RE_margin * ratio + RE) %>% 
  select(-patients, -ratio, -STANDARD_NAME, -CV_margin, -DM_margin, -RE_margin)

write.xlsx(eagle.full, '03_Outputs/Eagle_Potential_Internal.xlsx')


