# mapping 标准库
# all 待匹配数据

##---- Sound Shape Code Stock ----
# SSC
ssc.stock <- read_lines('02_Inputs/zh_data/hanzi_ssc_res.txt', 
                        progress = show_progress()) %>% 
  as.data.frame() %>% 
  rename('ssc_stock' = '.') %>% 
  separate(ssc_stock, c('id', 'character', 'ssc'), sep = '\t')

# mapping
mapping.format <- mapping %>% 
  mutate(mole = tolower(MOLE_NAME_CH), 
         mole = trimws(mole), 
         mole = gsub("[()]|'|,|[-]| |~|%|～|/|[[]|[]]|[.]|[+]", '', mole), 
         mole = gsub('viii', '8', mole), 
         mole = gsub('ix', '9', mole), 
         mole = gsub('vii', '7', mole), 
         mole = gsub('ii', '2', mole), 
         mole = gsub('i', '1', mole), 
         prod = tolower(PROD_NAME_CH), 
         prod = trimws(prod), 
         prod = gsub("[()]|'|,|[-]| |　|%|/|[\u00A0]", '', prod), 
         prod = gsub('ⅷ', '8', prod), 
         prod = gsub('viii', '8', prod), 
         prod = gsub('ii', '2', prod), 
         dosage = trimws(DOSAGE))

all.format <- all %>% 
  mutate(mole = tolower(ORIGIN_MOLE_NAME), 
         mole = trimws(mole), 
         mole = gsub(',|[-]', '', mole), 
         # mole = gsub('α', 'A', mole), 
         mole = gsub('viii', '8', mole), 
         mole = gsub('ix', '9', mole), 
         mole = gsub('ii', '2', mole), 
         mole = gsub('ⅱ', '2', mole), 
         prod = tolower(ORIGIN_PRODUCT_NAME), 
         prod = trimws(prod), 
         prod = gsub("[()]|[（）]|[-]| |/|[\u00A0]", '', prod), 
         prod = gsub('ⅷ', '8', prod), 
         dosage = trimws(ORIGIN_DOSAGE), 
         dosage = case_when(
           dosage == 'CAP' ~ '胶囊', 
           dosage == '鼻用喷雾剂' ~ '鼻喷剂/气雾剂', 
           dosage == '肠溶缓释胶囊' ~ '缓释胶囊', 
           dosage == '肠溶缓释片' ~ '缓释片', 
           dosage == '滴瓶剂' ~ '滴剂', 
           dosage == '滴丸' ~ '丸剂', 
           dosage == '冻干粉针剂' ~ '粉针剂', 
           dosage == '粉剂（粉剂针）' ~ '粉剂针', 
           dosage == '干糖浆剂' ~ '糖浆', 
           dosage == '缓释混悬剂' ~ '混悬剂', 
           dosage == '缓释颗粒剂' ~ '颗粒剂', 
           dosage == '缓释植入剂' ~ '植入剂', 
           dosage == '混悬液' ~ '混悬剂', 
           dosage == '胶浆剂' ~ '凝胶剂', 
           dosage == '胶囊剂' ~ '胶囊', 
           dosage == '胶囊剂（胶丸、滴丸）' ~ '胶囊', 
           dosage == '静脉输液' ~ '注射液', 
           dosage == '颗粒剂/冲剂' ~ '颗粒剂', 
           dosage == '口服混悬液' ~ '混悬剂', 
           dosage == '口服浓溶液' ~ '口服液', 
           dosage == '口服溶液剂' ~ '口服液', 
           dosage == '膜剂/药膜剂' ~ '膜剂', 
           dosage == '凝胶' ~ '凝胶剂', 
           dosage == '气雾剂（喷雾剂）' ~ '气雾剂/喷雾剂', 
           dosage == '溶液剂（注射剂）' ~ '注射液', 
           dosage == '乳剂' ~ '乳膏剂', 
           dosage == '软膏' ~ '乳膏剂', 
           dosage == '软胶囊(胶丸)' ~ '软胶囊', 
           dosage == '糖浆剂' ~ '糖浆', 
           dosage == '糖丸' ~ '丸剂', 
           dosage == '丸剂（蜜丸）' ~ '丸剂', 
           dosage == '细粒剂/细粉剂' ~ '颗粒剂', 
           dosage == '眼药水片/滴眼用片' ~ '滴眼剂', 
           TRUE ~ dosage
         ))

# molecule
mapping.mole <- mapping %>% 
  mutate(mole = tolower(MOLE_NAME_CH), 
         mole = trimws(mole), 
         mole = gsub("[()]|'|,|[-]| |~|%|～|/|[[]|[]]|[.]|[+]", '', mole), 
         # mole = gsub('α', 'A', mole), 
         # mole = gsub('β', 'B', mole), 
         # mole = gsub('γ', 'Y', mole), 
         # mole = gsub('ω', 'W', mole), 
         mole = gsub('viii', '8', mole), 
         mole = gsub('ix', '9', mole), 
         mole = gsub('vii', '7', mole), 
         mole = gsub('ii', '2', mole), 
         mole = gsub('i', '1', mole)) %>% 
  distinct(mole)

all.mole <- all %>% 
  mutate(mole = tolower(ORIGIN_MOLE_NAME), 
         mole = trimws(mole), 
         mole = gsub(',|[-]', '', mole), 
         # mole = gsub('α', 'A', mole), 
         mole = gsub('viii', '8', mole), 
         mole = gsub('ix', '9', mole), 
         mole = gsub('ii', '2', mole), 
         mole = gsub('ⅱ', '2', mole)) %>% 
  distinct(mole)

# chk <- gsub('[\u4e00-\u9fa50-9]', '', unlist(mapping.mole)) %>% 
#   unique() %>% 
#   paste(collapse = '') %>% 
#   str_split('', simplify = TRUE) %>% 
#   as.character() %>% 
#   unique()

mapping.mole.ssc <- mapping.mole %>% 
  arrange(mole) %>% 
  sapply(str_split, pattern = '') %>% 
  lapply(
    function(x) {
      data.frame(mole = paste(x, collapse = ''), 
                 character = x)
    }
  ) %>% 
  bind_rows() %>% 
  left_join(ssc.stock, by = 'character') %>% 
  mutate(ssc = if_else(is.na(ssc), character, ssc))

all.mole.ssc <- all.mole %>% 
  arrange(mole) %>% 
  sapply(str_split, pattern = '') %>% 
  lapply(
    function(x) {
      data.frame(mole = paste(x, collapse = ''), 
                 character = x)
    }
  ) %>% 
  bind_rows() %>% 
  left_join(ssc.stock, by = 'character') %>% 
  mutate(ssc = if_else(is.na(ssc), character, ssc))

# product
mapping.prod <- mapping %>% 
  mutate(prod = tolower(PROD_NAME_CH), 
         prod = trimws(prod), 
         prod = gsub("[()]|'|,|[-]| |　|%|/|[\u00A0]", '', prod), 
         prod = gsub('ⅷ', '8', prod), 
         prod = gsub('viii', '8', prod), 
         prod = gsub('ii', '2', prod)) %>% 
  distinct(prod)

all.prod <- all %>% 
  mutate(prod = tolower(ORIGIN_PRODUCT_NAME), 
         prod = trimws(prod), 
         prod = gsub("[()]|[（）]|[-]| |/|[\u00A0]", '', prod), 
         prod = gsub('ⅷ', '8', prod)) %>% 
  distinct(prod)

chk <- gsub('[\u4e00-\u9fa50-9]', '', unlist(all.prod)) %>% 
  unique() %>% 
  paste(collapse = '') %>% 
  str_split('', simplify = TRUE) %>% 
  as.character() %>% 
  unique()

mapping.prod.ssc <- mapping.prod %>% 
  arrange(prod) %>% 
  sapply(str_split, pattern = '') %>% 
  lapply(
    function(x) {
      data.frame(prod = paste(x, collapse = ''), 
                 character = x)
    }
  ) %>% 
  bind_rows() %>% 
  left_join(ssc.stock, by = 'character') %>% 
  mutate(ssc = if_else(is.na(ssc), character, ssc))

all.prod.ssc <- all.prod %>% 
  arrange(prod) %>% 
  sapply(str_split, pattern = '') %>% 
  lapply(
    function(x) {
      data.frame(prod = paste(x, collapse = ''), 
                 character = x)
    }
  ) %>% 
  bind_rows() %>% 
  left_join(ssc.stock, by = 'character') %>% 
  mutate(ssc = if_else(is.na(ssc), character, ssc))


##---- Match ----
cl <- makeCluster(10)
registerDoParallel(cl)

system.time(mole.match <- ddply(all.mole.ssc[, c(1, 4)], 
                                .(mole), 
                                summarise, 
                                start_id = KMPIndex(ssc, mapping.mole.ssc$ssc, 0.8), 
                                .progress = 'text'))

system.time(prod.match <- ddply(all.prod.ssc[1:199, c(1, 4)], 
                                .(prod), 
                                summarise, 
                                start_id = KMPIndex(ssc, mapping.prod.ssc$ssc, 0.8), 
                                .progress = 'text'))

stopCluster(cl)

