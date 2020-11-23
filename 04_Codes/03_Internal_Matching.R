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


##---- Matching further ----
## internal
internal.further <- read.xlsx('02_Inputs/BU THC机构销售数据.xlsx', sheet = '2019FY') %>% 
  filter(`2019FY销量` > 0) %>% 
  pivot_wider(id_cols = c(`省份`, `城市`, `CountyNameC`, `机构名称`), 
              names_from = `治疗领域`, 
              values_from = `2019FY销量`, 
              values_fill = 0) %>% 
  arrange(-CV) %>% 
  mutate(cv_cum = cumsum(CV) / sum(CV), 
         cv_flag = if_else(cv_cum <= 0.95, 1, 0)) %>% 
  arrange(-DM) %>% 
  mutate(dm_cum = cumsum(DM) / sum(DM), 
         dm_flag = if_else(dm_cum <= 0.95, 1, 0)) %>% 
  arrange(-RE) %>% 
  mutate(re_cum = cumsum(RE) / sum(RE), 
         re_flag = if_else(re_cum <= 0.95, 1, 0)) %>% 
  filter(cv_flag == 1 | dm_flag == 1 | re_flag == 1) %>% 
  filter(!(`机构名称` %in% name.mapping$NAME))

internal.namelist <- internal.further %>% 
  mutate(CountyNameC = if_else(CountyNameC == '其他', '', CountyNameC), 
         `机构名称` = gsub('中心卫生院|卫生院|分院|公立', '', `机构名称`)) %>% 
  unite(col = namelist, `省份`, `城市`, CountyNameC, `机构名称`, sep = '') %>% 
  mutate(namelist = grep('[\u4e00-\u9fa5]', namelist, value = TRUE), 
         namelist = gsub('省|市|区|县|乡|镇|村|街道|地区|生态|旅游|度假|经济|开发|技术|科技|商业|特色|高新|产业|综合|试验', '', namelist), 
         namelist = gsub('[()]|[A-Za-z0-9]|[\']|[-]|[（]|[）]|[*]|[、]|[?]|[[]|[]]|[〗]|[]|[]', '', namelist)) %>% 
  select(namelist) %>% 
  unlist() %>% 
  lapply(function(x) {
    data.frame(namelist = stri_paste(unique(c(str_split(x, pattern = '', simplify = TRUE))), collapse = ''))
  }) %>% 
  bind_rows() %>% 
  bind_cols(internal.further) %>% 
  select(namelist, CV, DM, RE)

## standard
division.further <- eagle.potential.internal %>% 
  filter(is.na(CV), is.na(DM), is.na(RE), !is.na(`机构名称`))

division.further.m <- eagle.potential.internal %>% 
  filter(!(is.na(CV) & is.na(DM) & is.na(RE) & !is.na(`机构名称`)))

division.namelist <- division.further %>% 
  mutate(`机构名称` = gsub('中心卫生院|卫生院|分院|公立', '', `机构名称`)) %>% 
  unite(col = namelist, Province, City, Prefecture, `机构名称`, sep = '') %>% 
  mutate(namelist = grep('[\u4e00-\u9fa5]', namelist, value = TRUE), 
         namelist = gsub('省|市|区|县|乡|镇|村|街道|地区|生态|旅游|度假|经济|开发|技术|科技|商业|特色|高新|产业|综合|试验', '', namelist), 
         namelist = gsub('[()]|[A-Za-z0-9]|[\']|[-]|[（]|[）]|[*]|[、]|[?]|[[]|[]]|[〗]|[]|[]', '', namelist)) %>% 
  select(namelist) %>% 
  unlist() %>% 
  lapply(function(x) {
    data.frame(namelist = stri_paste(unique(c(str_split(x, pattern = '', simplify = TRUE))), collapse = ''))
  }) %>% 
  bind_rows() %>% 
  bind_cols(division.further) %>% 
  select(-CV, -DM, -RE)

## matching
further.matching <- internal.namelist %>% 
  left_join(division.namelist, by = 'namelist')

write.xlsx(further.matching, '05_Internal_Review/Internal_Matching_Result_95.xlsx')
write.xlsx(division.namelist, '05_Internal_Review/Division_Namelist.xlsx')


##---- Manual matching ----
manual.mapping1 <- read.xlsx('05_Internal_Review/Internal_Matching_part1.xlsx')
manual.mapping2 <- read.xlsx('05_Internal_Review/Internal_Matching_part2.xlsx')

manual.mapping <- bind_rows(manual.mapping1, manual.mapping2) %>% 
  mutate(STANDARD_NAME = if_else(is.na(STANDARD_NAME), STANDARD_NAME1, STANDARD_NAME)) %>% 
  filter(!is.na(STANDARD_NAME)) %>% 
  group_by(STANDARD_NAME) %>% 
  summarise(CV = sum(CV, na.rm = TRUE), 
            DM = sum(DM, na.rm = TRUE), 
            RE = sum(RE, na.rm = TRUE)) %>% 
  ungroup()

manual.matching <- division.further %>% 
  select(-CV, -DM, -RE) %>% 
  left_join(manual.mapping, by = 'STANDARD_NAME') %>% 
  bind_rows(division.further.m)

write.xlsx(manual.matching, '03_Outputs/Eagle_Potential_Inernal.xlsx')

## check
pft.check <- eagle.potential.division %>% 
  filter(is.na(`机构名称`)) %>% 
  distinct(Province, City, Prefecture) %>% 
  mutate(flag = 1) %>% 
  right_join(bu.thc, by = c('Province' = '省份', 'City' = '城市', 'Prefecture' = 'CountyNameC')) %>% 
  distinct(Province, City, Prefecture, flag) %>% 
  mutate(flag = if_else(is.na(flag), 0, flag)) %>% 
  filter(flag == 1)

manual.matching %>% select(CV, DM, RE) %>% colSums(na.rm = TRUE)

bu.thc %>% 
  left_join(pft.check, by = c('省份' = 'Province', '城市' = 'City', 'CountyNameC' = 'Prefecture')) %>% 
  filter(is.na(flag)) %>% 
  select(CV, DM, RE) %>% 
  colSums(na.rm = TRUE)







