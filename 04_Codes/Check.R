


bu.thc.mapping.raw <- read.xlsx('02_Inputs/CHC医院匹配结果.xlsx')

bu.thc.mapping <- bu.thc.mapping.raw %>% 
  add_count(ID) %>% 
  filter(n == 1) %>% 
  select(-AREA, -YEAR, -`2019_FY_SALES`, -ID, -n) %>% 
  distinct()

chk <- bu.thc.mapping %>% 
  add_count(NAME) %>% 
  filter(n > 1)

write.xlsx(bu.thc.mapping, '03_Outputs/BU_THC_Mapping.xlsx')



