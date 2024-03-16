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
  
turnout18 <- read.csv('Data/2018_voted.csv',sep = ';',dec = ',')
# countries proportions ---------------------------------------------------
RU04_20 <- read.csv("Data/RU04_20_Russia.csv")

RU_total <- RU04_20 %>% 
  # filter(region == "Europe") %>%
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(year) %>% 
  summarise(total = sum(number)) 

# cumulative add to total voters
RU_cumsum <- RU04_20 %>% 
  # filter(region == "Europe") %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(en_country,year) %>% 
  summarise(cntr_total = sum(number)) %>% 
  arrange(-cntr_total) %>% 
  left_join(RU_total) %>% 
  mutate(ratio = round((cntr_total/total)*100,4)) %>%
  group_by(year) %>% 
  mutate(cum_rat = cumsum(ratio),cum_num = cumsum(cntr_total),index = row_number()) 

# cumulative graph
# RU_cumsum %>% 
#   ggplot(aes(x=index,y=cum_rat))+
#   geom_point(aes(color=ifelse(en_country == "Austria",'red','grey70')))+
#   facet_grid(~year)+
#   scale_color_identity()+
#   theme_minimal_hgrid()+
#   panel_border()


g1 <- RU_cumsum %>% 
  filter(cum_rat <= 75) %>% 
  filter(year==2018) %>% 
  # arrange(year,-index) %>%
  mutate(en_country = if_else(grepl("America",en_country),"USA",en_country)) %>%
  # View()
  ggplot(aes(
    text = paste0(
      "страна: ", en_country,
      "<br>кол-во проголосовавших: ", cntr_total,
      "<br>от всех избирателей: ", round(ratio,0),"%",
      "<br>накопленный процент: ", round(cum_rat,0),"%"
    )))+
  geom_col(aes(y=ratio,x=reorder(en_country,-ratio)))+
  geom_line(aes(y=cum_rat,group=1,x=reorder(en_country,-ratio)))+
             # facet_grid(.~year)+
             theme_cowplot()+
             panel_border()+
             labs(y="",x="")+
  theme(axis.text.x = element_text(angle=90))
         

ggplotly(g1,tooltip = 'text')


RU_cumsum %>% 
  filter(cum_rat <= 75) %>% 
  group_by(year) %>% 
  summarize(n = max(index)) %>%
  ggplot(aes(x=year,y=n))+
  geom_col()
