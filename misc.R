RU04_20 %>% 
  filter(year == 2018,en_country == 'Latvia',
         Label=='Число действительных избирательных бюллетеней') %>% 
  # View()
  summarise(sum = sum(number))



RU04_20 %>% 
  filter(year == 2018,
         region == 'Europe',
         Label=='Число действительных избирательных бюллетеней') %>% 
  group_by(en_country) %>% 
  summarise(sum = sum(number)) %>% 
  arrange(-sum) %>% 
  write.csv('Data/2018_voted.csv',row.names = FALSE)
  