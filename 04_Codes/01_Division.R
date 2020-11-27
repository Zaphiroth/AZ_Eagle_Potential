# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ Eagle potential
# Purpose:      Potential division
# programmer:   Zhe Liu
# Date:         2020-11-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin ----
heath.center <- read_excel('02_Inputs/2019年中国卫生院数据库clean version.xls')
eagle.potential <- read_excel('02_Inputs/AZ_Eagle_Potential_Format.xlsx')

## heath center update
heath.center.city.check <- heath.center %>% 
  distinct(`省`, `地级市`) %>% 
  add_count(`地级市`) %>% 
  filter(n > 1) %>% 
  arrange(`地级市`)

heath.center.district.check <- heath.center %>% 
  distinct(`省`, `地级市`, `区[县/县级市]`) %>% 
  add_count(`区[县/县级市]`) %>% 
  filter(n > 1) %>% 
  arrange(`区[县/县级市]`, `地级市`)

# write.xlsx(heath.center.city.check, '05_Internal_Review/Center_City_Error.xlsx')
# write.xlsx(heath.center.district.check, '05_Internal_Review/Center_District_Error.xlsx')

sub.pft <- read.xlsx('02_Inputs/Sub_Prefecture.xlsx')

heath.center.update <- heath.center %>% 
  mutate(Province = gsub('省|市|自治区|回族', '', `省`)) %>% 
  left_join(sub.pft, by = c('Province', '区[县/县级市]' = 'Prefecture')) %>% 
  mutate(`省` = if_else(`地级市` == '西宁', '青海省', `省`), 
         `地级市` = if_else(`地级市` %in% c('省直辖县级行政区划', '自治区直辖县级行政区划'), 
                         `区[县/县级市]`, 
                         `地级市`), 
         `地级市` = if_else(!is.na(City), City, `地级市`), 
         `区[县/县级市]` = case_when(
           `区[县/县级市]` == '湄潭县' ~ '湄潭区', 
           `区[县/县级市]` == '南和县' ~ '南和区', 
           `区[县/县级市]` == '南岔县' ~ '南岔区', 
           `区[县/县级市]` == '监利县' ~ '监利市', 
           `区[县/县级市]` == '邵东县' ~ '邵东市', 
           `区[县/县级市]` == '海门市' ~ '海门区', 
           `区[县/县级市]` == '龙南县' ~ '龙南市', 
           `区[县/县级市]` == '同仁县' ~ '同仁市', 
           `区[县/县级市]` == '蓬莱市' ~ '蓬莱区', 
           `区[县/县级市]` == '太谷县' ~ '太谷区', 
           `区[县/县级市]` == '新津县' ~ '新津区', 
           `区[县/县级市]` == '和田市' ~ '和田县', 
           TRUE ~ `区[县/县级市]`
         ))

## eagle potential check
eagle.potential.check <- eagle.potential %>% 
  add_count(Province, City, Prefecture) %>% 
  filter(n > 1) %>% 
  arrange(Province, City, Prefecture)

write.xlsx(eagle.potential.check, '05_Internal_Review/Eagle_Repetitive_District.xlsx')


##---- Potential division ----
## division ratio
division.ratio <- heath.center.update %>% 
  distinct(`TM编码`, 
           Province = gsub('省|市|自治区|回族', '', `省`), 
           City = gsub('市', '', `地级市`), 
           Prefecture = `区[县/县级市]`, 
           `机构名称`, 
           `地址`, 
           `邮编`, 
           patients = `总诊疗人次数`) %>% 
  group_by(Province, City, Prefecture, `机构名称`) %>% 
  summarise(`TM编码` = first(`TM编码`), 
            `地址` = first(`地址`), 
            `邮编` = first(na.omit(`邮编`)), 
            patients = sum(patients, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Province, City, Prefecture) %>% 
  mutate(ratio = patients / sum(patients)) %>% 
  ungroup()

chk <- division.ratio %>% 
  group_by(Province, City, Prefecture) %>% 
  summarise(x = round(sum(ratio), 2)) %>% 
  ungroup()

## division
eagle.potential.division <- eagle.potential %>% 
  mutate(City = gsub('市', '', City)) %>% 
  group_by(Province, City, Prefecture) %>% 
  summarise(`潜力合计` = sum(`潜力合计`, na.rm = TRUE), 
            `内部销量合计` = sum(`内部销量合计`, na.rm = TRUE), 
            `潜力新分组` = first(`潜力新分组`), 
            `内部销量新分组` = first(`内部销量新分组`), 
            `Segment-New` = first(`Segment-New`), 
            `MS%` = sum(`MS%`, na.rm = TRUE), 
            `份额分组` = first(`份额分组`), 
            Decile = first(Decile), 
            `潜力分组` = first(`潜力分组`), 
            `份额高低` = first(`份额高低`), 
            `区县分类` = first(`区县分类`), 
            CV1 = sum(CV1, na.rm = TRUE), 
            DM1 = sum(DM1, na.rm = TRUE), 
            RE1 = sum(RE1, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(division.ratio, by = c('Province', 'City', 'Prefecture')) %>% 
  mutate(ratio = if_else(is.na(ratio), 1, ratio), 
         CV1 = CV1 * ratio, 
         DM1 = DM1 * ratio, 
         RE1 = RE1 * ratio)

write.xlsx(eagle.potential.division, '03_Outputs/Eagle_Potential_Division.xlsx')

## potential
eagle.potential.fmt <- eagle.potential %>% 
  mutate(City = gsub('市', '', City)) %>% 
  group_by(Province, City, Prefecture) %>% 
  summarise(`潜力合计` = sum(`潜力合计`, na.rm = TRUE), 
            `内部销量合计` = sum(`内部销量合计`, na.rm = TRUE), 
            `潜力新分组` = first(`潜力新分组`), 
            `内部销量新分组` = first(`内部销量新分组`), 
            `Segment-New` = first(`Segment-New`), 
            `MS%` = sum(`MS%`, na.rm = TRUE), 
            `份额分组` = first(`份额分组`), 
            Decile = first(Decile), 
            `潜力分组` = first(`潜力分组`), 
            `份额高低` = first(`份额高低`), 
            `区县分类` = first(`区县分类`), 
            CV1 = sum(CV1, na.rm = TRUE), 
            DM1 = sum(DM1, na.rm = TRUE), 
            RE1 = sum(RE1, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(division.ratio, by = c('Province', 'City', 'Prefecture'))

write.xlsx(eagle.potential.fmt, '03_Outputs/Eagle_Potential_Format.xlsx')
