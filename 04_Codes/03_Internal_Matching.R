# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ Eagle potential
# Purpose:      Potential division
# programmer:   Zhe Liu
# Date:         2020-11-17
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
## internal sales
internal <- read.xlsx('02_Inputs/BU THC机构销售数据.xlsx', sheet = '2020YTD08') %>% 
  mutate(`城市` = gsub('市', '', `城市`), 
         `YTD202008销售金额` = if_else(`治疗领域` == 'RE', `2019年销售金额`, `YTD202008销售金额`)) %>% 
  filter(`YTD202008销售金额` > 0) %>% 
  pivot_wider(id_cols = c(`省份`, `城市`, CountyName, `机构名称`), 
              names_from = `治疗领域`, 
              values_from = `YTD202008销售金额`, 
              values_fill = 0) %>% 
  mutate(CV = CV * 1.5 * 1.21 * 1.15, 
         DM = DM * 1.5 * 1.23 * 1.15, 
         RE = RE * 1.21 * 1.12)

## heath center
name.mapping <- read.xlsx('02_Inputs/Sim_Double_Check.xlsx', sheet = 2, cols = c(6, 12, 19:23)) %>% 
  filter(flag0 == 1) %>% 
  filter(NAME %in% internal$`机构名称`) %>% 
  distinct(NAME, STANDARD_NAME) %>% 
  filter(!(NAME %in% c('高青县青城卫生院')))

## joint
# internal.mapping <- internal %>% 
#   inner_join(name.mapping, by = c('机构名称' = 'NAME')) %>% 
#   group_by(STANDARD_NAME) %>% 
#   summarise(CV = sum(CV, na.rm = TRUE), 
#             DM = sum(DM, na.rm = TRUE), 
#             RE = sum(RE, na.rm = TRUE)) %>% 
#   ungroup()


##---- Matching ----
eagle.potential.fmt <- read.xlsx('03_Outputs/Eagle_Potential_Format.xlsx')

eagle.potential.internal <- eagle.potential.fmt %>% 
  left_join(name.mapping, by = 'STANDARD_NAME')

# write.xlsx(eagle.potential.internal, '03_Outputs/Eagle_Potential_Inernal.xlsx')

## heath center
# name.mapping <- read.xlsx('02_Inputs/Sim_Double_Check.xlsx', sheet = 2, cols = c(6, 12, 19:23)) %>% 
#   filter(flag0 == 1) %>% 
#   filter(NAME %in% internal$`机构名称`) %>% 
#   distinct(NAME, STANDARD_NAME) %>% 
#   filter(!(NAME %in% c('高青县青城卫生院')))


##---- Full mapping ----
## internal sales
# internal <- read.xlsx('02_Inputs/BU THC机构销售数据.xlsx', sheet = '2020YTD08') %>% 
#   mutate(`城市` = gsub('市', '', `城市`), 
#          `YTD202008销售金额` = if_else(`治疗领域` == 'RE', `2019年销售金额`, `YTD202008销售金额`)) %>% 
#   filter(`YTD202008销售金额` > 0) %>% 
#   pivot_wider(id_cols = c(`省份`, `城市`, CountyName, `机构名称`), 
#               names_from = `治疗领域`, 
#               values_from = `YTD202008销售金额`, 
#               values_fill = 0) %>% 
#   mutate(CV = CV * 1.5 * 1.21 * 1.15, 
#          DM = DM * 1.5 * 1.23 * 1.15, 
#          RE = RE * 1.21 * 1.12)

## IT
part.it <- internal %>% 
  inner_join(name.mapping, by = c('机构名称' = 'NAME')) %>% 
  filter(STANDARD_NAME %in% eagle.potential.internal$STANDARD_NAME) %>% 
  group_by(Province_I = `省份`, City_I = `城市`, Prefecture_I = CountyName, 
           NAME = `机构名称`, STANDARD_NAME) %>% 
  summarise(CV = sum(CV, na.rm = TRUE), 
            DM = sum(DM, na.rm = TRUE), 
            RE = sum(RE, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(flag = 'IT')

## manual1
part.manual1 <- bind_rows(manual.mapping1, manual.mapping2) %>% 
  mutate(`城市` = gsub('市', '', `城市`), 
         STANDARD_NAME = if_else(is.na(STANDARD_NAME), STANDARD_NAME1, STANDARD_NAME)) %>% 
  distinct(`省份`, `城市`, CountyNameC, `机构名称` = `机构名称.x`, STANDARD_NAME) %>% 
  left_join(internal, by = c('省份', '城市', 'CountyNameC' = 'CountyName', '机构名称')) %>% 
  filter(!is.na(STANDARD_NAME)) %>% 
  distinct(`机构名称`, .keep_all = TRUE) %>% 
  group_by(Province_I = `省份`, City_I = `城市`, Prefecture_I = CountyNameC, 
           NAME = `机构名称`, STANDARD_NAME) %>% 
  summarise(CV = sum(CV, na.rm = TRUE), 
            DM = sum(DM, na.rm = TRUE), 
            RE = sum(RE, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(flag = 'Manual')

## manual2
part.manual2 <- bind_rows(manual.mapping3, manual.mapping4) %>% 
  mutate(`城市` = gsub('市', '', `城市`)) %>% 
  distinct(`省份`, `城市`, CountyNameC, `机构名称`, STANDARD_NAME) %>% 
  left_join(internal, by = c('省份', '城市', 'CountyNameC' = 'CountyName', '机构名称')) %>% 
  filter(!is.na(STANDARD_NAME)) %>% 
  group_by(Province_I = `省份`, City_I = `城市`, Prefecture_I = CountyNameC, 
           NAME = `机构名称`, STANDARD_NAME) %>% 
  summarise(CV = sum(CV, na.rm = TRUE), 
            DM = sum(DM, na.rm = TRUE), 
            RE = sum(RE, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(flag = 'Manual')

## surplus
part.surplus <- internal %>% 
  filter(!(`机构名称` %in% part.it$NAME), 
         !(`机构名称` %in% part.manual1$NAME), 
         !(`机构名称` %in% part.manual2$NAME)) %>% 
  select(Province_I = `省份`, City_I = `城市`, Prefecture_I = CountyName, 
         NAME = `机构名称`, CV, DM, RE) %>% 
  mutate(flag = 'Unmatched')

## standard total
internal.std1 <- bind_rows(part.it, part.manual1, part.manual2, part.surplus)

## update standard name
std.update <- read.xlsx('05_Internal_Review/Internal_Std_Update.xlsx') %>% 
  distinct(Province_I, City_I, Prefecture_I, NAME, STANDARD_NAME_m = STANDARD_NAME)

internal.std2 <- internal.std1 %>% 
  left_join(std.update, by = c('Province_I', 'City_I', 'Prefecture_I', 'NAME')) %>% 
  mutate(STANDARD_NAME = if_else(!is.na(STANDARD_NAME_m), STANDARD_NAME_m, STANDARD_NAME)) %>% 
  select(-STANDARD_NAME_m)

## update RE
re.update.hc <- read.xlsx('05_Internal_Review/RE_Update_New_HC.xlsx', cols = c(2, 6))

re.update <- read_xlsx('02_Inputs/【Data4】2019salesbyTAupdate 原始内部销量.xlsx', 
                       sheet = 'CH739_20200929_195157') %>% 
  filter(`机构类型...18` == '卫生院', `治疗领域` == 'RE') %>% 
  mutate(`城市` = gsub('市', '', `城市`)) %>% 
  group_by(Province_I = `省份`, NAME = `机构名称`) %>% 
  summarise(`城市` = first(na.omit(`城市`)), 
            InsCountyNameC = first(na.omit(InsCountyNameC)), 
            RE_m = sum(`销售金额`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(RE_m > 0)

internal.std3 <- internal.std2 %>% 
  full_join(re.update, by = c('Province_I', 'NAME')) %>% 
  mutate(RE_m = if_else(is.na(RE_m), 0, RE_m), 
         RE = RE_m * 1.21 * 1.12, RE, 
         CV = if_else(is.na(CV), 0, CV), 
         DM = if_else(is.na(DM), 0, DM), 
         City_I = if_else(is.na(City_I), `城市`, City_I), 
         Prefecture_I = if_else(is.na(Prefecture_I), InsCountyNameC, Prefecture_I)) %>% 
  left_join(re.update.hc, by = 'NAME') %>% 
  mutate(STANDARD_NAME = if_else(is.na(STANDARD_NAME), STANDARD_NAME_m, STANDARD_NAME), 
         flag = if_else(is.na(flag) & is.na(STANDARD_NAME), 'Unmatched', flag), 
         flag = if_else(is.na(flag) & !is.na(STANDARD_NAME), 'Manual', flag)) %>% 
  select(-`城市`, -InsCountyNameC, -RE_m, -STANDARD_NAME_m)

# chk <- re.update %>% 
#   filter(!(NAME %in% internal.std2$NAME))
# 
# write.xlsx(chk, 'RE_Update_New_HC.xlsx')

## write out
write.xlsx(internal.std3, '03_Outputs/Internal_Standard.xlsx')


##---- Matching further ----
## internal
internal.further <- read.xlsx('02_Inputs/BU THC机构销售数据.xlsx', sheet = '2019FY') %>% 
  filter(`2019FY销量` > 0) %>% 
  pivot_wider(id_cols = c(`省份`, `城市`, `CountyName`, `机构名称`), 
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
  mutate(CountyName = if_else(CountyName == '其他', '', CountyName), 
         `机构名称` = gsub('中心卫生院|卫生院|分院|公立', '', `机构名称`)) %>% 
  unite(col = namelist, `省份`, `城市`, CountyName, `机构名称`, sep = '') %>% 
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
  right_join(bu.thc, by = c('Province' = '省份', 'City' = '城市', 'Prefecture' = 'CountyName')) %>% 
  distinct(Province, City, Prefecture, flag) %>% 
  mutate(flag = if_else(is.na(flag), 0, flag)) %>% 
  filter(flag == 1)

manual.matching %>% select(CV, DM, RE) %>% colSums(na.rm = TRUE)

bu.thc %>% 
  left_join(pft.check, by = c('省份' = 'Province', '城市' = 'City', 'CountyName' = 'Prefecture')) %>% 
  filter(is.na(flag)) %>% 
  select(CV, DM, RE) %>% 
  colSums(na.rm = TRUE)


##---- Manual matching 2nd ----
## manual matching table
internal.further2 <- read.xlsx('02_Inputs/BU THC机构销售数据.xlsx', sheet = '2019FY') %>% 
  filter(`2019FY销量` > 0) %>% 
  pivot_wider(id_cols = c(`省份`, `城市`, `CountyName`, `机构名称`), 
              names_from = `治疗领域`, 
              values_from = `2019FY销量`, 
              values_fill = 0) %>% 
  filter(!(`机构名称` %in% internal.further$`机构名称`)) %>% 
  filter(!(`机构名称` %in% name.mapping$NAME))

write.xlsx(internal.further2, '05_Internal_Review/Internal_Matching2.xlsx')

## manual matching
manual.mapping3 <- read.xlsx('05_Internal_Review/Internal_Matching2_part1.xlsx')
manual.mapping4 <- read.xlsx('05_Internal_Review/Internal_Matching2_part2.xlsx')

manual.mapping.m <- bind_rows(manual.mapping3, manual.mapping4) %>% 
  filter(!is.na(STANDARD_NAME)) %>% 
  group_by(STANDARD_NAME) %>% 
  summarise(CV = sum(CV, na.rm = TRUE), 
            DM = sum(DM, na.rm = TRUE), 
            RE = sum(RE, na.rm = TRUE)) %>% 
  ungroup()

manual.matching2 <- manual.matching %>% 
  filter(is.na(CV)) %>% 
  select(-CV, -DM, -RE) %>% 
  left_join(manual.mapping.m, by = 'STANDARD_NAME') %>% 
  bind_rows(manual.matching[!is.na(manual.matching$CV), ])

manual.matching2 %>% 
  filter(!is.na(CV)) %>% 
  summarise(CV = sum(CV), 
            DM = sum(DM), 
            RE = sum(RE))


