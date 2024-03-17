pu <- tibble(year=c(2004,2008,2012,2018,2020),
             rat=c(71.3,70.28,63.6,76.7,78.45),
             en_country = rep("Официальный результат 
выборов в РФ",5)) %>% 
  mutate(rat_dec = rat*0.01)

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



# all votes ---------------------------------------------------------------
voters_total <- function(sel_region = NA, sel_country = NA){
  
  if(is.na(sel_region)){
    sel_region = all_of(RU04_20$region)
    sel_region_label = NULL
  }  else {
    sel_region
    sel_region_label = sel_region
  }
  
  if(is.na(sel_country)){
    sel_country = all_of(RU04_20$en_country) 
    sel_country_label = NULL
  } else {
    sel_country
    sel_country_label = sel_country
  }  
  
  total_voters <- RU04_20 %>% 
    filter(region %in% c(sel_region)) %>% 
    filter(en_country %in% c(sel_country)) %>%
    filter(Label %in% c("Число недействительных избирательных бюллетеней",
                        "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
    group_by(year) %>% 
    summarise(total = sum(number))
  
  
  gov_voters <- RU04_20 %>% 
    filter(region %in% c(sel_region)) %>% 
    filter(en_country %in% c(sel_country)) %>%
    filter(Label %in% c("yes",
                        "Путин Владимир Владимирович",
                        "Медведев Дмитрий Анатольевич"
    )) %>%
    group_by(year) %>% 
    summarize(gov_total = sum(number))
  
  reject_voters <- RU04_20 %>% 
    filter(region == sel_region) %>% 
    filter(en_country %in% c(sel_country)) %>%
    filter(Label %in% c("Rejected.ballots",
                        "Число недействительных избирательных бюллетеней")) %>% 
    group_by(year) %>% 
    summarize(reject_total = sum(number))
  
  against_voters <- RU04_20 %>% 
    filter(region %in% c(sel_region)) %>% 
    filter(en_country %in% c(sel_country)) %>%
    filter(Label %in% c("Глазьев Сергей Юрьевич",
                        "Против всех",
                        "Малышкин Олег Александрович",
                        "Миронов Сергей Михайлович",
                        "Хакамада Ирина Муцуовна",
                        "Харитонов Николай Михайлович",
                        "Богданов Андрей Владимирович",
                        "Жириновский Владимир Вольфович",
                        "Зюганов Геннадий Андреевич",
                        "Прохоров Михаил Дмитриевич",
                        "Бабурин Сергей Николаевич",
                        "Грудинин Павел Николаевич",
                        "Собчак Ксения Анатольевна",
                        "Сурайкин Максим Александрович",
                        "Титов Борис Юрьевич",
                        "Явлинский Григорий Алексеевич",
                        "no")) %>% 
    group_by(year) %>% 
    summarize(against_total = sum(number))
  
  
  sum_voters <- total_voters %>% 
    left_join(gov_voters) %>% 
    left_join(reject_voters) %>% 
    left_join(against_voters) %>% 
    mutate(gov_ratio = gov_total/total,
           reject_ratio = reject_total/total,
           against_ratio = against_total/total)
  
  
  
  g1 <- sum_voters %>% 
    select(year,gov_total,reject_total,against_total) %>% 
    pivot_longer(-year,names_to = "category",values_to = 'ratio') %>% 
    mutate(category = case_when(
      category == "gov_total" ~ "про-правительственный голос",
      category == "against_total" ~ "голос против правительства",
      category == "reject_total" ~ "испорченный",
      TRUE~category
    )) %>% 
    ggplot()+
    geom_bar(aes(x=year,
                 y=ratio,
                 fill=category), 
                 position = 'fill', 
                 stat = 'identity')+
    geom_point(data = pu,
               aes(x=year,y=rat_dec),
               size = 4,
               alpha = 0.9,
               color = 'yellow'
    ) +
    scale_fill_manual(values = c('#0084D7','#005082','#8c190f'))+
    theme_cowplot()+
    theme_minimal_hgrid()+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    labs(y="",x="",fill="",title=paste0("Процент голосующих ",sel_region_label,"  ",sel_country_label))+
    theme(legend.position = 'top',
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())+
    scale_x_continuous(breaks = c(2004,2008,2012,2018,2020))
  
  ggplotly(g1) %>%  
    plotly::layout(legend=list(x=0, 
                               xanchor='left',
                               yanchor='top',
                               orientation='h'))
}

voters_total()
voters_total(sel_country = 'Austria')
voters_total(sel_country = 'Germany')
voters_total(sel_region = 'Europe')



# total voters ------------------------------------------------------------

total_voters <- function(sel_region = NA, sel_country = NA){
if(is.na(sel_region)){
  sel_region = all_of(RU04_20$region)
  sel_region_label = NULL
}  else {
  sel_region
  sel_region_label = sel_region
}

if(is.na(sel_country)){
  sel_country = all_of(RU04_20$en_country) 
  sel_country_label = NULL
} else {
  sel_country
  sel_country_label = sel_country
}  
total_voters <- RU04_20 %>% 
  filter(region %in% c(sel_region)) %>%
  filter(en_country %in% c(sel_country)) %>%
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(year) %>% 
  summarise(total = sum(number))

g1 <- total_voters %>% 
  ggplot(aes(x=year,y=total))+
  geom_bar(position = 'stack',stat='identity')+
  geom_text(aes(label=total,y=total*0.92),vjust = 1.5,color='grey80')+
  theme_cowplot()+
  theme_minimal_hgrid()+
  labs(y="",x="",fill="",title=paste0("Кол-во проголосовавших ",sel_region_label,"  ",sel_country_label),
       subtitle= "Кол-во проголосовавших за рубежом")+
  scale_x_continuous(breaks = c(2004,2008,2012,2018,2020))+
  theme(legend.position = 'top',
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.subtitle = element_text(size=12,color='grey70',hjust=1))
# g1
ggplotly(g1)
}
options(scipen = 999)
total_voters()
total_voters(sel_country = 'Austria')

RU04_20 %>% 
  distinct(en_country)
# c("Число избирателей, включенных в списки избирателей",
#     "Число действительных избирательных бюллетеней",
#     "Ballots.in.box",
#     "yes",
#     "no",
#     "Против всех",
#     "Rejected.ballots",
#     "Число недействительных избирательных бюллетеней",
#     "Глазьев Сергей Юрьевич",
#     "Малышкин Олег Александрович",
#     "Миронов Сергей Михайлович",
#     "Путин Владимир Владимирович",
#     "Хакамада Ирина Муцуовна",
#     "Харитонов Николай Михайлович",
#     "Богданов Андрей Владимирович",
#     "Жириновский Владимир Вольфович",
#     "Зюганов Геннадий Андреевич",
#     "Медведев Дмитрий Анатольевич",
#     "Прохоров Михаил Дмитриевич",
#     "Бабурин Сергей Николаевич",
#     "Грудинин Павел Николаевич",
#     "Собчак Ксения Анатольевна",
#     "Сурайкин Максим Александрович",
#     "Титов Борис Юрьевич",
#     "Явлинский Григорий Алексеевич"
#   )


# turnout vs result -------------------------------------------------------

# clean up data, leave only PU and ME
RU04_20_PM <-
  RU04_20 %>% 
  filter(str_detect(Label,'Пу|Ме|yes')) %>% 
  group_by(en_country,year,region) %>% 
  summarise(rat = mean(ratio)) %>% 
  # filter(region == "Europe") %>%
  # distinct(en_country) %>% View()
  mutate(color = case_when(
    en_country == "Austria" ~ "red",
    en_country == "Netherlands" ~ "forestgreen",
    TRUE ~"grey80")
  )
RU18_res <- RU04_20_PM %>% filter(year==2018) %>% 
  select(en_country,year,rat) %>% 
  mutate(result = rat*0.01) %>% 
  select(-rat)

turn18_clean <- turnout18 %>% 
  filter(!is.na(vote_ratio)) %>% 
  mutate(turnout = vote_ratio) %>% 
  select(en_country, turnout)
  

turnout_results18 <- turn18_clean %>%  
  left_join(RU18_res)

g1 <- turnout_results18 %>% 
  ggplot(aes(x=turnout,y=result,label = en_country))+
  geom_point()+
  geom_smooth(method = 'lm')+
  theme_minimal_grid()
  # geom_abline(slope = 1)
# g1
ggplotly(g1)

fit <- lm(result~turnout, turnout_results18)
summary(fit)

