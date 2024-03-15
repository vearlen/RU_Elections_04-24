RU04_20 %>% 
  filter(year == 2018,en_country == 'Czechia',
         Label=='Число действительных избирательных бюллетеней') %>% 
  # View()
  summarise(sum = sum(number))
