# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ Eagle potential
# Purpose:      Potential division
# programmer:   Zhe Liu
# Date:         2020-11-13
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin AZ info ----
## BU THC
bu.thc <- read.xlsx('02_Inputs/BU THC机构销售数据.xlsx', sheet = '2019FY') %>% 
  filter(`2019FY销量` > 0) %>% 
  pivot_wider(id_cols = -c(`治疗领域`, `2019FY销量`), 
              names_from = `治疗领域`, 
              values_from = `2019FY销量`, 
              values_fill = 0) %>% 
  mutate(`城市` = gsub('市', '', `城市`), 
         `机构名称` = gsub('\\d', '', `机构名称`))

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


##---- Name list ----
## SSC
ssc.stock <- read_lines('02_Inputs/zh_data/hanzi_ssc_res.txt', 
                        progress = show_progress()) %>% 
  as.data.frame() %>% 
  rename('ssc_stock' = '.') %>% 
  separate(ssc_stock, c('id', 'character', 'ssc'), sep = '\t')

## BU THC
bu.thc.namelist <- bu.thc %>% 
  mutate(CountyNameC = if_else(CountyNameC == '其他', '', CountyNameC), 
         `机构名称` = gsub('中心卫生院|卫生院', '', `机构名称`)) %>% 
  unite(col = namelist, `省份`, `城市`, CountyNameC, `机构名称`, sep = '') %>% 
  mutate(namelist = grep('[\u4e00-\u9fa5]', namelist, value = TRUE), 
         namelist = gsub('市|区|县|乡|镇|村|[()]|[A-Za-z]|[-]|[_]|[/]', '', namelist)) %>% 
  distinct(namelist) %>% 
  unlist() %>% 
  lapply(str_split, pattern = '', simplify = TRUE) %>% 
  lapply(as.character) %>% 
  lapply(unique) %>% 
  lapply(
    function(x) {
      data.frame(namelist = paste(x, collapse = ''), 
                 character = x) %>% 
        arrange(character)
    }
  ) %>% 
  bind_rows() %>% 
  left_join(ssc.stock, by = 'character')

## Eagle potential
eagle.potential.namelist <- eagle.potential.division %>% 
  filter(!is.na(`机构名称`)) %>% 
  mutate(`机构名称` = gsub('中心卫生院|卫生院', '', `机构名称`), 
         `机构名称` = trimws(`机构名称`)) %>% 
  unite(col = namelist, Province, City, Prefecture, `机构名称`, sep = '') %>% 
  mutate(namelist = grep('[\u4e00-\u9fa5]', namelist, value = TRUE), 
         namelist = gsub('市|区|县|乡|镇|村|生态|旅游|经济|开发|技术|科技|商业|特色|高新|产业|综合|试验', '', namelist), 
         namelist = gsub('[()]|[A-Za-z0-9]|[\']|[-]|[（]|[）]|[*]|[、]|[?]|[[]|[]]|[〗]|[]|[]', '', namelist)) %>% 
  distinct(namelist) %>% 
  unlist() %>% 
  lapply(str_split, pattern = '', simplify = TRUE) %>% 
  lapply(as.character) %>% 
  lapply(unique) %>% 
  lapply(
    function(x) {
      data.frame(namelist = paste(x, collapse = ''), 
                 character = x) %>% 
        arrange(character)
    }
  ) %>% 
  bind_rows() %>% 
  left_join(ssc.stock, by = 'character')


##---- KMP ----
source('04_Codes/KMP.R')

cl <- makeCluster(10)
registerDoParallel(cl)

system.time(name.match <- ddply(bu.thc.namelist[1:102, c(1, 4)], 
                                .(namelist), 
                                summarise, 
                                start_id = KMPIndex(ssc, eagle.potential.namelist$ssc, 0.8), 
                                .progress = 'text'))

stopCluster(cl)




