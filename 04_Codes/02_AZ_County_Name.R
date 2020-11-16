# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ Eagle potential
# Purpose:      Potential division
# programmer:   Zhe Liu
# Date:         2020-11-13
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin AZ info ----
## BU THC
bu.thc <- read.xlsx('02_Inputs/BU THC机构销售数据.xlsx', sheet = '2019FY') %>% 
  pivot_wider(id_cols = -c(`治疗领域`, `2019FY销量`), 
              names_from = `治疗领域`, 
              values_from = `2019FY销量`, 
              values_fill = 0) %>% 
  mutate(`城市` = gsub('市', '', `城市`), 
         `机构名称` = gsub('\\d', '', `机构名称`), 
         CountyNameC = case_when(
           CountyNameC == '临夏县' ~ '临夏市'
         ))

bu.thc.org <- bu.thc %>% 
  mutate(org = rm_between(`机构名称`, '(', ')'), 
         org = gsub('.*县', '', org), 
         org = gsub('中心卫生院|卫生院', '', org)) %>% 
  add_count(`省份`, `城市`, `CountyNameC`, org) %>% 
  filter(n == 1)

## prefecture mapping
bu.thc.pft <- bu.thc %>% 
  distinct(Province = `省份`, City = `城市`, Prefecture = CountyNameC) %>% 
  mutate(flag = 1)

eagle.potential.pft <- eagle.potential.division %>% 
  distinct(Province, City, Prefecture) %>% 
  left_join(bu.thc.pft) %>% 
  filter(is.na(flag))



##---- Mapping ----
eagle.potential.null <- eagle.potential.division %>% 
  filter(!(stri_paste(Province, City, Prefecture) %in% 
             stri_paste(bu.thc$`省份`, bu.thc$`城市`, bu.thc$CountyNameC))) %>% 
  distinct(Province, City, Prefecture)

eagle.potential.thc1 <- eagle.potential.division %>% 
  filter(stri_paste(Province, City, Prefecture) %in% 
           stri_paste(bu.thc$`省份`, bu.thc$`城市`, bu.thc$CountyNameC)) %>% 
  left_join(bu.thc, by = c('Province' = '省份', 'City' = '城市', 
                           'Prefecture' = 'CountyNameC', '机构名称'))

eagle.potential.thc2 <- eagle.potential.thc1 %>% 
  filter(is.na(`机构代码`)) %>% 
  select_if(~sum(!is.na(.)) > 0) %>% 
  mutate(org = rm_between(`机构名称`, '(', ')'), 
         org = gsub('.*县', '', org), 
         org = gsub('中心卫生院|卫生院', '', org)) %>% 
  left_join(bu.thc.org, by = c('Province' = '省份', 'City' = '城市', 
                               'Prefecture' = 'CountyNameC', 'org'))

eagle.potential.thc3 <- eagle.potential.thc2 %>% 
  filter(is.na(`机构代码`))


chk <- bu.thc.org %>% 
  add_count(`省份`, `城市`, `CountyNameC`, org) %>% 
  filter(n > 1)
filter(!(CountyNameC %in% eagle.potential.division$Prefecture))
