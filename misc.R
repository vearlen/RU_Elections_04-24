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



# RU04_20 %>% 
#   filter(year == 2018,
#          region == 'Europe',
#          Label=='Число действительных избирательных бюллетеней') %>% 
#   group_by(en_country) %>% 
#   summarise(sum = sum(number)) %>% 
#   arrange(-sum) %>% 
#   write.csv('Data/2018_voted.csv',row.names = FALSE)
#   
turnout18 <- read.csv('Data/2018_voted.csv',sep = ';',dec = ',')
RU04_20 <- read.csv("Data/RU04_20_Russia.csv")
# countries proportions ---------------------------------------------------

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




# Europe ------------------------------------------------------------------
eu_cntr <- RU04_20 %>% 
  filter(region == "Europe") %>% 
  distinct(en_country) %>% 
  filter(!en_country %in% c("Moldova","Ukraine","Belarus")) %>% 
  unlist()


eu_cntr
add_cntr <- c(eu_cntr,"Israel","United States of America","Canada","Vietnam")
cntr_list <- c(eu_cntr,add_cntr)
# clean up data, leave only PU and ME
RU04_20_PM <-
  RU04_20 %>% 
  filter(str_detect(Label,'Пу|Ме|yes')) %>% 
  group_by(en_country,year,region) %>% 
  summarise(rat = mean(ratio)) %>% 
  # filter(region == "Europe") %>%
  mutate(color = case_when(
    en_country == "Austria" ~ "red",
    en_country == "Netherlands" ~ "forestgreen",
    TRUE ~"grey80")
  )

test <- RU04_20_PM %>% 
  filter(year==2012) %>% 
  distinct(en_country) %>% tail() %>% list()


# test[[1]][6,2]


g1 <- RU04_20_PM %>%
  ggplot(aes(x = year, y = rat, label = en_country)) +
  geom_hline(yintercept = 50,
             linewidth = 0.2,
             color = 'grey20') +
  geom_text_repel(
    data = filter(RU04_20_PM,
                  year == 2012,
                  en_country %in% c("Austria")),
    color = 'red',
    segment.curvature = -0.1,
    segment.ncp = 1,
    segment.angle = 90,
    box.padding = 3,
    min.segment.length = 0,
    size=5,
    # fontface='bold',
    nudge_x = -0.1
  ) +
  geom_text_repel(
    data = filter(RU04_20_PM,
                  year == 2012,
                  en_country %in% c(test[[1]][6,2])),
    color = 'grey30',
    size=5,
    segment.curvature = -0.5,
    segment.ncp = 4,
    segment.angle = 90,
    box.padding = 1.4,
    min.segment.length = 1,
    nudge_x = -0.1,
    # nudge_y = -1,
    segment.shape=1
  ) +
  geom_text_repel(
    data = filter(pu,
                  year == 2012),
    color = '#636201',
    size=5,
    nudge_x=1,
    nudge_y=-8,
    hjust=0,
    segment.curvature = -0.1,
    segment.ncp = 2,
    segment.angle = 90,
    box.padding = 1,
    min.segment.length = 0
  ) +
  geom_jitter(
    aes(group = en_country),
    width = 0.2,
    size = 2.5,
    alpha =0.9,
    color = 'grey10',
    shape=21,
    fill = 'grey80'
  ) +
  geom_star(
    data = pu,
    color = 'brown4',
    size = 6,
    alpha = 0.9,
    fill = 'yellow'
  ) +
  geom_jitter(
    data = filter(RU04_20_PM, en_country == "Austria"),
    aes(group = en_country),
    width = 0,
    size = 4,
    alpha = 0.8,
    color = 'black',
    fill = 'red',
    shape = 21
  ) +
  geom_jitter(
    data = filter(RU04_20_PM, en_country == test[[1]][6,2]),
    aes(group = en_country),
    width = 0,
    height = 0,
    size = 4,
    alpha = 0.8,
    color = 'black',
    fill = 'green3',
    shape = 21
  ) +
  theme_minimal() +
  scale_color_identity() +
  scale_x_continuous(breaks = c(2004, 2008, 2012, 2018, 2020)) +
  scale_y_continuous(breaks = seq(20, 100, by = 10)) +
  labs(
    x = "",
    y = "%",
    title = "Как Европа голосовала на выборах президента РФ, референдуме 2020",
    subtitle = "каждая точка - процент голосов в отдельной европейской стране за Путина/Медведева
и за изменение конституции в 2020 году",
    caption = "Источник данных: ЦИК России
Сбор данных и визуализация: И. Тищенко"
  ) +
  
  theme(
    axis.line.x = element_line(color = 'grey80', linewidth = .0),
    axis.ticks.x = element_line(color = 'grey80', linewidth = 0.0),
    axis.text.x.bottom = element_text(size = 12, color = 'grey50',
                                      margin = margin(-10,10,0,40)) ,
    axis.text.y = element_text(size = 12),
    panel.grid = element_blank(),
    axis.title.y = element_text(
      angle = 0,
      vjust=0.95,
      hjust=0.1,
      size=13,
      margin = margin (0,-12,0,20)),
    # panel.grid.major.x = element_line(color='grey70',linetype =5, linewidth = 0.3),
    panel.grid.major.y = element_line(
      color = 'grey80',
      linetype = 5,
      linewidth = 0.3
    ),
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0, size = 17, face = 'bold'),
    plot.caption = element_text(
      size = 12,
      color = 'grey50',
      face =  'italic',
      margin = margin(0, 0, 0, 0)
    ),
    plot.subtitle = element_text(
      size = 14,
      color = 'grey30',
      hjust = 0
    ),
    legend.background = element_rect(fill = alpha('white', 0.8), colour = 'white'),
    legend.title = element_blank()
  )

g1


# exit poll ---------------------------------------------------------------
exit24_prelim <- read.csv('Data/exit_poll_2024_prelim.csv',sep=';')
  
RU_total_UIK <- RU04_20 %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(UIK,year,en_country,Location) %>% 
  summarise(total = sum(number)) 

ext_uik_list <- exit24_prelim %>% 
  distinct(Voting.station) %>% unlist()

exit_cl <- exit24_prelim %>% 
  separate(Country,c("ru_country","en_country"),"/") %>% 
  separate(City,c("ru_city","en_city"),"/") %>% 
  fill(en_country) %>% 
  fill(en_city) %>% 
  mutate(en_country = str_trim(en_country)) %>% 
  mutate(ru_city = str_trim(ru_city)) %>% 
  select(-ru_country,-en_city) %>% 
  mutate(en_country = case_when(
    en_country == "Great Britain" ~ "United Kingdom",
    en_country == "Federal Republic of Germany" ~ "Germany",
    TRUE ~ en_country)) %>% 
  select(en_country, Location = ru_city,UIK = Voting.station, Surveyed = Voters.surveyed,Exit = Voters.counted.at.the.exit) %>% 
  mutate(total = if_else(is.na(Exit),Surveyed,Exit),
         year = 2024)
  

  
g1 <- RU_total_UIK %>% 
  filter(year %in% c(2018)) %>% 
  bind_rows(exit_cl) %>% 
  filter(UIK %in% ext_uik_list) %>%
  mutate(Location = str_trim(Location)) %>%
  pivot_wider(id_cols = c(UIK,en_country,Location),names_from = year,values_from = total) %>% 
  mutate(diff = `2024`- `2018`, ratio = round(diff/`2018`*100,0)) %>% 
  filter(!is.na(ratio)) %>% 

  ggplot(aes(x=reorder(Location,-ratio),y=ratio))+
  geom_point(aes(
    text = paste0(
      en_country,
      '<br>',
      Location,
      '<br>2018: ',
      `2018`,
      '<br>2024: ',
      `2024`,
      '<br>',
      ratio,'%'
    )
  ),color='#0084D7')+
  coord_flip()+
  # annotate('text',x=100,y=0,label = "Явка 2024 показана по результатам экзит пола,
  # Внимание! Это не всегда полная явка, иногда это кол-во опрошенных, когда нет данных о всех.")+
  labs(x="",y="% разницы с 2018 годом",title = "Сравнение явки на выборах 2018 и 2024 года.",
       caption = "Явка 2024 показана по результатам экзит пола,
Внимание! Это не всегда полная явка, иногда это кол-во опрошенных, когда нет данных о всех.")+
  theme_minimal()+
  theme(plot.caption = element_text(size=10,hjust=0,vjust=0))
g1

ggplotly(g1, tooltip = 'text') %>% 
layout(annotations = list(x = 50, y = 40, text = "Явка 2024 показана по результатам экзит пола,
Внимание! Это не всегда полная явка, иногда это кол-во опрошенных, когда нет данных о всех
                          https://voteabroad.info/",
                            # xref='paper', yref='paper', 
                          showarrow = F, xanchor='left', yanchor='left', xshift=0, yshift=0,
                            font = list(size = 12)))

