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
         `地级市` = if_else(!is.na(City), City, `地级市`))

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
  summarise(CV1 = sum(CV1, na.rm = TRUE), 
            DM1 = sum(DM1, na.rm = TRUE), 
            RE1 = sum(RE1, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(division.ratio, by = c('Province', 'City', 'Prefecture')) %>% 
  mutate(CV1_center = CV1 * ratio, 
         DM1_center = DM1 * ratio, 
         RE1_center = RE1 * ratio) %>% 
  select(-CV1, -DM1, -RE1, -patients, -ratio)

write.xlsx(eagle.potential.division, '03_Outputs/Eagle_Potential_Division.xlsx')


##---- Check ----
chk <- eagle.potential.division %>% 
  filter(is.na(ratio)) %>% 
  distinct(Province, City, Prefecture) %>% 
  arrange(Province, City, Prefecture) %>% 
  filter(!(City %in% c('北京', '上海')))

write.xlsx(chk, 'Sub_Prefecture.xlsx')

chk1 <- eagle.potential.division %>% 
  mutate(flag = if_else(is.na(ratio), 0, 1)) %>% 
  group_by(flag) %>% 
  summarise(CV1 = sum(CV1, na.rm = TRUE), 
            DM1 = sum(DM1, na.rm = TRUE), 
            RE1 = sum(RE1, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate()


