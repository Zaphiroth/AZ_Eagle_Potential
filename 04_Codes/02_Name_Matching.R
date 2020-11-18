# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  AZ Eagle potential
# Purpose:      Name matching
# programmer:   Zhe Liu
# Date:         2020-11-13
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Distinct ----
bu.thc.mapping.raw <- read.xlsx('02_Inputs/CHC医院匹配结果.xlsx')

bu.thc.mapping <- bu.thc.mapping.raw %>% 
  add_count(ID) %>% 
  filter(n == 1) %>% 
  select(-AREA, -YEAR, -`2019_FY_SALES`, -ID, -n) %>% 
  distinct()

chk <- bu.thc.mapping %>% 
  add_count(NAME) %>% 
  filter(n > 1)

write.xlsx(bu.thc.mapping, '03_Outputs/BU_THC_Mapping_Result.xlsx')


##---- Diff ----
## name character list
name.list <- bu.thc.mapping %>% 
  mutate(NAME = paste0(PROVINCE, CITY, CountyNameC, NAME), 
         NAME = gsub('中心|卫生院', '', NAME), 
         NAME = grep('[\u4e00-\u9fa5]', NAME, value = TRUE), 
         NAME = gsub('省|市|区|县|乡|镇|村|生态|旅游|经济|开发|技术|科技|商业|特色|高新|产业|综合|试验', '', NAME), 
         NAME = gsub('[()]|[A-Za-z0-9]|[\']|[-]|[（]|[）]|[*]|[、]|[?]|[[]|[]]|[〗]|[]|[]', '', NAME), 
         STANDARD_NAME = paste0(PROVINCE, CITY, DISTRICT, STANDARD_NAME), 
         STANDARD_NAME = gsub('中心|卫生院', '', STANDARD_NAME), 
         STANDARD_NAME = grep('[\u4e00-\u9fa5]', STANDARD_NAME, value = TRUE), 
         STANDARD_NAME = gsub('省|市|区|县|乡|镇|村|生态|旅游|经济|开发|技术|科技|商业|特色|高新|产业|综合|试验', '', STANDARD_NAME), 
         STANDARD_NAME = gsub('[()]|[A-Za-z0-9]|[\']|[-]|[（]|[）]|[*]|[、]|[?]|[[]|[]]|[〗]|[]|[]', '', STANDARD_NAME)) %>% 
  select(NAME, STANDARD_NAME) %>% 
  lapply(str_split, pattern = '') %>% 
  lapply(lapply, unique)

## plus diff
diff.plus <- list()

for (i in 1:length(name.list[[1]])) {
  diff.plus[[i]] <- setdiff(name.list[[2]][[i]], name.list[[1]][[i]])
}

diff.plus <- diff.plus %>% 
  lapply(function(x) {
    data.frame(diff_plus = stri_paste(x, collapse = ''))
  }) %>% 
  bind_rows()

## minus diff
diff.minus <- list()

for (i in 1:length(name.list[[1]])) {
  diff.minus[[i]] <- setdiff(name.list[[1]][[i]], name.list[[2]][[i]])
}

diff.minus <- diff.minus %>% 
  lapply(function(x) {
    data.frame(diff_minus = stri_paste(x, collapse = ''))
  }) %>% 
  bind_rows()

## similarity
sim.chk <- bind_cols(bu.thc.mapping, diff.plus, diff.minus) %>% 
  mutate(flag0 = if_else(nchar(diff_plus) + nchar(diff_minus) == 0, 1, 0), 
         flag1 = if_else(nchar(diff_plus) + nchar(diff_minus) <= 1, 1, 0), 
         flag2 = if_else(nchar(diff_plus) + nchar(diff_minus) <= 2, 1, 0))

write.xlsx(sim.chk, '05_Internal_Review/Sim_Double_Check.xlsx')


##---- SSC ----
name <- bu.thc.mapping %>% 
  select(NAME) %>% 
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
  left_join(ssc.stock, by = 'character') %>% 
  filter(!is.na(ssc))

name.std <- bu.thc.mapping %>% 
  select(STANDARD_NAME) %>% 
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
  left_join(ssc.stock, by = 'character') %>% 
  filter(!is.na(ssc))






