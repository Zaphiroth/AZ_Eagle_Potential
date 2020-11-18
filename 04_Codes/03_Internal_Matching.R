# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ Eagle potential
# Purpose:      Potential division
# programmer:   Zhe Liu
# Date:         2020-11-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
## internal sales
internal <- read.xlsx('02_Inputs/BU THC机构销售数据.xlsx', sheet = '2019FY') %>% 
  filter(`2019FY销量` > 0) %>% 
  pivot_wider(id_cols = `机构名称`, 
              names_from = `治疗领域`, 
              values_from = `2019FY销量`, 
              values_fill = 0)

## heath center
name.mapping <- read.xlsx('02_Inputs/Sim_Double_Check.xlsx', sheet = 2, cols = c(6, 12, 19:23)) %>% 
  filter(flag0 == 1) %>% 
  filter(NAME %in% internal$`机构名称`) %>% 
  distinct(NAME, STANDARD_NAME) %>% 
  filter(!(NAME %in% c('高青县青城卫生院')))

## joint
internal.mapping <- internal %>% 
  inner_join(name.mapping, by = c('机构名称' = 'NAME')) %>% 
  group_by(STANDARD_NAME) %>% 
  summarise(CV = sum(CV, na.rm = TRUE), 
            DM = sum(DM, na.rm = TRUE), 
            RE = sum(RE, na.rm = TRUE)) %>% 
  ungroup()


##---- Matching ----
eagle.potential.internal <- eagle.potential.division %>% 
  mutate(STANDARD_NAME = stri_paste(Prefecture, `机构名称`)) %>% 
  left_join(internal.mapping, by = 'STANDARD_NAME')

write.xlsx(eagle.potential.internal, '03_Outputs/Eagle_Potential_Inernal.xlsx')
