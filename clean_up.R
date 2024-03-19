library(tidyverse)

RU04_24 <- read.csv('Data/RU04_24_Russia.csv')

RU04_24 %>% 
  filter(year == 2018) %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(year) %>% 
  summarise(total = sum(number))

# 468565+5801

# 2024
354577+26147

RU04_24 %>% 
  filter(year == 2024) %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(year) %>% 
  summarise(total = sum(number))

RU24_list <- RU04_24 %>% 
  filter(year == 2024) %>% 
  distinct(UIK) %>% 
  arrange(UIK) %>% 
  mutate(UIK = as.numeric(UIK))
  # names()
  # group_by(year) %>% 




UIK24_CIK <- read.csv('Data/2024_UIK_CIK_source.txt',header = FALSE)
UIK24_CIK_lst <- str_replace_all(UIK24_CIK$V1,'УИК №','') %>% tibble(UIK = .) %>% mutate(UIK = as.numeric(UIK))


test <- anti_join(UIK24_CIK_lst,RU24_list)

missUIK24 <- read.csv('Data/missingUIK2024.csv',sep=';',check.names = FALSE)

misUIK24_long <- missUIK24 %>% 
  pivot_longer(cols=-Label,names_to = 'UIK',values_to = 'number') %>% 
  mutate(UIK = as.integer(UIK))

misUIK24_cntr <- misUIK24_long %>% left_join(RU04_20_distinct_cntr,by = join_by(UIK)) %>% mutate(year = 2024)

misUIK24_cntr %>% 
  filter(is.na(country)) %>% 
  View()

RU04_24_all <-  bind_rows(RU04_24,misUIK24_cntr)


RU04_24_all %>% 
  filter(year == 2024) %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней")) %>%
  # View()
  group_by(year) %>% 
  summarise(total = sum(number))
354577+26147

write.csv(RU04_24_all,'Data/RU04_24_Russia_corr.csv')
