library(tidyverse)

RU04_24 <- read.csv('Data/RU04_24_Russia.csv')

# clean up ----------------------------------------------------------------


# RU04_24 %>% 
#   filter(year == 2024) %>% 
#   filter(str_detect(Label,"Пут|Дав|Слуц|Хар")) %>% 
#   filter(is.na(ratio)) %>% 
#   # distinct(UIK) %>% 
#   View()
# 
# 
# mis_UIK_ratio <- read.csv("Data/missingUIK2024_ratio.csv",sep = ";",check.names = FALSE)
# 
# mis_UIK_key <- mis_UIK_ratio %>% 
#   pivot_longer(cols = -Label,names_to = "UIK",values_to = "ratio") %>% 
#   mutate(year = 2024) %>% 
#   unite(key, UIK,year,Label)
# 
# RU04_24_key <- RU04_24 %>% 
#   unite(key,UIK,year,Label) %>% 
#   select(-X) 
# 
# test <- rows_patch(RU04_24_key,mis_UIK_key,by='key') %>% 
#   separate(key,into = c("UIK","year","Label"),sep = "_")

# RU24_ratio_upd <- RU04_24 %>% 
#   filter(year == 2024) %>% 
#   filter(str_detect(Label,'%')) %>% 
#   mutate(Label = str_replace(Label,"\\(%\\)","")) %>%
#   select(UIK,year,Label,ratio = number) %>% 
#   unite(key,UIK,year,Label) %>% 
#   mutate(key = str_trim(key))
# 
# # str(RU04_24)
# # str(RU24_ratio)
# RU04_24_key <- RU04_24 %>% 
#   filter(year == 2024) %>% 
#   filter(!str_detect(Label,'%')) %>% 
#   unite(key, UIK, year,Label) %>% 
#   mutate(key = str_trim(key)) %>% 
#   select(-X)
# 
# RU24_ratio <- rows_patch(RU04_24_key,RU24_ratio_upd,by='key') # %>% 
#   separate(key,into = c("UIK","year","Label"),sep = "_") %>% filter(!is.na(ratio)) %>% 
#   mutate(UIK = is.integer(UIK))
# 
# RU04_24_clean <- RU04_24 %>% 
#   # filter(year == 2024) %>% 
#   filter(!str_detect(Label,'%')) %>% 
#   unite(key,UIK,year,Label)
# 
# test <- rows_patch(RU04_24_clean,RU24_ratio,by='key') %>% 
#   separate(key,into = c("UIK","year","Label"),sep = "_")
# 
# 
# 
# test %>% 
#   filter(year == 2024) %>% 
#   filter(Label %in% c("Число недействительных избирательных бюллетеней",
#                                             "Число действительных избирательных бюллетеней","Ballots.in.box")) %>%
#   group_by(year) %>% 
#   summarise(sum = sum(number))

# write.csv(test,'Data/RU04_24_Russia.csv',row.names = FALSE)



# cleaning ----------------------------------------------------------------

# 
# 
# RU04_24 %>% 
#   filter(year == 2018) %>% 
#   filter(Label %in% c("Число недействительных избирательных бюллетеней",
#                       "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
#   group_by(year) %>% 
#   summarise(total = sum(number))
# 
# # 468565+5801
# 
# # 2024
# 354577+26147
# 
# RU04_24 %>% 
#   filter(year == 2024) %>% 
#   filter(Label %in% c("Число недействительных избирательных бюллетеней",
#                       "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
#   group_by(year) %>% 
#   summarise(total = sum(number))
# 
# RU24_list <- RU04_24 %>% 
#   filter(year == 2024) %>% 
#   distinct(UIK) %>% 
#   arrange(UIK) %>% 
#   mutate(UIK = as.numeric(UIK))
#   # names()
#   # group_by(year) %>% 
# 
# 
# 
# 
# UIK24_CIK <- read.csv('Data/2024_UIK_CIK_source.txt',header = FALSE)
# UIK24_CIK_lst <- str_replace_all(UIK24_CIK$V1,'УИК №','') %>% tibble(UIK = .) %>% mutate(UIK = as.numeric(UIK))
# 
# 
# test <- anti_join(UIK24_CIK_lst,RU24_list)
# 
# missUIK24 <- read.csv('Data/missingUIK2024.csv',sep=';',check.names = FALSE)
# 
# misUIK24_long <- missUIK24 %>% 
#   pivot_longer(cols=-Label,names_to = 'UIK',values_to = 'number') %>% 
#   mutate(UIK = as.integer(UIK))
# 
# misUIK24_cntr <- misUIK24_long %>% left_join(RU04_20_distinct_cntr,by = join_by(UIK)) %>% mutate(year = 2024)
# 
# misUIK24_cntr %>% 
#   filter(is.na(country)) %>% 
#   View()
# 
# RU04_24_all <-  bind_rows(RU04_24,misUIK24_cntr)
# 
# 
# RU04_24_all %>% 
#   filter(year == 2024) %>% 
#   filter(Label %in% c("Число недействительных избирательных бюллетеней",
#                       "Число действительных избирательных бюллетеней")) %>%
#   # View()
#   group_by(year) %>% 
#   summarise(total = sum(number))
# # 354577+26147
# 
# # write.csv(RU04_24_all,'Data/RU04_24_Russia.csv')
# 
# cntr_iso <- read.csv('Data/all_countries.csv', sep = ';')
# names(RU04_24_all)
# 
# RU04_24_corr <- RU04_24_all %>% 
#   filter(is.na(alpha.2)) %>%
#   filter(!en_country %in% c("Namibia","Osetia","Abkhazia")) %>% 
#   select(-c(alpha.2,alpha.3,region,sub.region)) %>% 
#   left_join(cntr_iso,by=join_by(en_country == name)) %>% 
#   select(Label,UIK,number,ratio,year,Location,country,en_country,alpha.2,alpha.3,region,sub.region) %>% 
#   distinct(.)
# 
# RU04_24_outlier <- RU04_24_all %>% 
#   filter(is.na(alpha.2)) %>%
#   filter(!en_country %in% c("Namibia","Osetia","Abkhazia")) 
# 
# RU04_24_wout_out <-  anti_join(RU04_24_all,RU04_24_outlier )
# 
# RU04_24_fin <-  bind_rows(RU04_24_wout_out ,RU04_24_corr)
# # RU04_24big <- RU04_24_all %>% 
# #   filter(!is.na(alpha.2)) %>% 
# #   filter(!en_country %in% c("Namibia","Osetia","Abkhazia")) %>%
#   # select(-c(alpha.2,alpha.3,region,sub.region)) %>% 
# 
# # 37898+360
# RU04_24_fin %>% 
#  write.csv('Data/RU04_24_Russia.csv')
# 
# 
# 
# Namibia
# Osetia
# Abkhazia