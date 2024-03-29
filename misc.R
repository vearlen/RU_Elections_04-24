library(tidyverse)
library(cowplot)
library(plotly)
library("gganimate")
library('gifski')
# import data -------------------------------------------------------------
RU04_24 <- read.csv('Data/RU04_24_Russia.csv')

pu <- tibble(year=c(2004,2008,2012,2018,2020,2024),
             rat=c(71.3,70.28,63.6,76.7,78.45,87),
             en_country = rep("Официальный результат 
выборов в РФ",6)) %>% 
  mutate(rat_dec = rat*0.01)


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
RU04_20 <- read.csv("Data/RU04_20_Russia.csv", sep = ';')
# RU24 elections ----------------------------------------------------------

ru_en_cntr <- read.csv('Data/countries_ru_en.csv', sep= ';',fileEncoding = 'UTF-8')

ru24_elections <- read.csv('Data/RU24_outside_elections.csv', dec = ',',check.names = FALSE)
# 
RU24_long <- ru24_elections %>%
  rename(UIK = УИК, country = Страна, Location = Город) %>%
  select(-"№") %>%
  pivot_longer(cols = -c(UIK,country,Location),
               names_to = "Label", values_to = "number")
# 

RU04_20_distinct_cntr <- RU04_20 %>% filter(year == 2018) %>% 
  distinct(UIK,en_country,country,alpha.2,alpha.3,region,sub.region)
# # 
RU24_long_cntr <-  RU24_long  %>%  
  left_join(RU04_20_distinct_cntr, by=join_by(UIK)) %>%
  mutate(year = 2024) 



# find where alpha.2 empty and add en country
RU24_all_cntr <- RU24_long_cntr %>%
  mutate(country.x = str_trim(country.x)) %>%
  left_join(ru_en_cntr,by = join_by(country.x == RU_NAME)) %>%
  mutate(en_country = if_else(is.na(en_country),EN_NAME,en_country)) %>%
  rename(country = country.x) %>%
  select(-c(EN_NAME,country.y))



# all_cntr <- read.csv('Data/all_countries.csv',sep = ";")

# all_cntr$name <- iconv(all_cntr$name, to="UTF-8")
# RU24_all_cntr$en_country <- iconv(RU24_all_cntr$en_country,to="UTF-8")

# RU24_all_cntr %>% 
#   select(year,en_country,number,Label,Location,UIK,country) %>% 
#   mutate_if(is.character,str_trim) %>%
#   left_join(all_cntr %>% mutate_if(is.character,str_trim),by = join_by(en_country == name)) %>% 
#   filter(is.na(alpha.2)) %>% 
#   distinct(en_country) %>% 
#   View()


# Encoding(all_cntr$name)
# Encoding(RU24_all_cntr$en_country)
names(RU24_all_cntr)
names(RU04_20)

RU04_24 <- RU04_20 %>%
  bind_rows(RU24_long_cntr) %>%
  mutate(country = case_when(
    is.na(country) ~ ifelse(is.na(country.y),country.x,country.y),
    TRUE ~country
  )) %>% 
  select(-c(country.x,country.y)) %>% 
  left_join(ru_en_cntr,by = join_by(country == RU_NAME)) %>% 
  mutate(en_country = if_else(is.na(en_country),EN_NAME,en_country)) %>% 
  select(-EN_NAME)
  
names(RU04_24)
RU04_24 %>% 
  filter(year ==2024, is.na(en_country)) %>% 
  # distinct(en_country,UIK) %>% 
  View()

write.csv(RU04_24,'Data/RU04_24_Russia.csv',row.names = FALSE)
# countries proportions ---------------------------------------------------



RU_total <- RU04_24 %>% 
  # filter(region == "Europe") %>%
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней",
                      "Ballots.in.box")) %>% 
  group_by(year) %>% 
  summarise(total = sum(number)) 

# cumulative add to total voters
RU_cumsum <- RU04_24 %>% 
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

cum_75plot <-  function(el_year=2004){
  g1 <- RU_cumsum %>% 
    filter(cum_rat <= 75) %>% 
    filter(year==el_year) %>% 
    mutate(en_country = if_else(grepl("America",en_country),"USA",en_country)) %>%
    ggplot(aes(
      text = paste0(
        "страна: ", en_country,
        "<br>кол-во проголосовавших: ", cntr_total,
        "<br>от всех избирателей: ", round(ratio,0),"%",
        "<br>накопленный процент: ", round(cum_rat,0),"%",
        "<br>накопленное кол-во проголосовавших/всего: ", cum_num," / ", total 
      )))+
    geom_col(aes(y=ratio,x=reorder(en_country,-ratio)),fill='#0084D7')+
    geom_line(aes(y=cum_rat,group=1,x=reorder(en_country,-ratio)),color='grey40')+
    geom_point(aes(y=cum_rat,x=reorder(en_country,-ratio)),color='grey40')+         
    theme_minimal_hgrid()+
    labs(y="",x="",title=el_year)+
    theme(axis.text.x = element_text(angle=90))
  
  
  ggplotly(g1,tooltip = 'text')
}
cum_75plot(el_year = 2024)

# distribution ------------------------------------------------------------


sel_region = "Asia"
sel_country = "Israel"
# voters total with deltas-----------------------------------------------------------
voters_total <- function(sel_region = NA, sel_country = NA, ratio=TRUE,plotly_plot = FALSE){
  
  if(is.na(sel_region)){
    sel_region = all_of(RU04_24$region)
    sel_region_label = "all regions"
      if(!is.na(sel_country)){sel_region_label = NULL}
  }  else {
    sel_region
    sel_region_label = sel_region
  }
  
  if(is.na(sel_country)){
    sel_country = all_of(RU04_24$en_country) 
    sel_country_label = NULL
  } else {
    sel_country
    sel_country_label = sel_country
  }  
  
  total_voters <- RU04_24 %>% 
    filter(region %in% c(sel_region)) %>% 
    filter(en_country %in% c(sel_country)) %>%
    filter(Label %in% c("Число недействительных избирательных бюллетеней",
                        "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
    group_by(year) %>% 
    summarise(total = sum(number))
  
  
  
  gov_voters <- RU04_24 %>% 
    filter(region %in% c(sel_region)) %>%
    filter(en_country %in% c(sel_country)) %>%
    filter(Label %in% c("yes",
                        "Путин Владимир Владимирович",
                        "Медведев Дмитрий Анатольевич"
    )) %>%
    group_by(year) %>% 
    summarize(gov_total = sum(number))
  
  reject_voters <- RU04_24 %>% 
    filter(region == sel_region) %>%
    filter(en_country %in% c(sel_country)) %>%
    filter(Label %in% c("Rejected.ballots",
                        "Число недействительных избирательных бюллетеней")) %>% 
    group_by(year) %>% 
    summarize(reject_total = sum(number))
  
  against_voters <- RU04_24 %>% 
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
                        "Даванков Владислав Андреевич",
                        "Слуцкий Леонид Эдуардович",
                        "Харитонов Николай Михайлович",
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
  
  
  
  sum_tmp <-  sum_voters %>% 
    select(year,gov_total,reject_total,against_total) %>% 
    pivot_longer(-year,names_to = "category",values_to = 'ratio') %>% 
    mutate(category = case_when(
      category == "gov_total" ~ "pro P",
      category == "against_total" ~ "against P",
      category == "reject_total" ~ "spoiled ballot",
      TRUE~category
    )) %>% 
    mutate(category = fct_relevel(category,'against P','spoiled ballot','pro P'))
  
pro_p_label <-
  sum_tmp %>% 
  filter(year != 2020) %>% 
  pivot_wider(id_cols = year,names_from = category,values_from = ratio) %>% 
  mutate(pro_p_label = `pro P` - dplyr::lag(`pro P`), 
         pro_p_x = (year - 1.5),
         pro_p_y = `pro P`,
         against_p_label = (`against P`+`spoiled ballot`) - 
                            (dplyr::lag(`spoiled ballot`)+dplyr::lag(`against P`)), 
         against_p_x = (year - 1.5),
         against_p_y = (`against P`+`pro P`+`spoiled ballot`)) %>% 
  mutate(seg_x = if_else(year>2004,year-1,NA),
         seg_xend = if_else(year>2004,year,NA)) %>% 
  rowwise() %>% 
  mutate(total = sum(`pro P`+`spoiled ballot`+`against P`)) %>% 
  mutate(pro_r = round(`pro P`/total,2),
         against_r = round(`against P`/total,2),
         spoiled_r = round(`spoiled ballot`/total,2)) %>% 
  mutate(mid_pro_y = `pro P`/2,
         mid_against_y = (`pro P`+`spoiled ballot` + `against P`)-(`against P`/2),
         mid_spoiled_y = (`pro P`+`spoiled ballot`)-(`spoiled ballot`/2))
  

# static plot with deltas
 g1 <-  sum_tmp %>% 
   filter(year != 2020) %>%
    ggplot()+
   geom_segment(
     data = pro_p_label,
     aes(
       x = seg_x,
       y = `pro P`,
       xend = seg_xend,
       yend = `pro P`
     ),
     color = 'brown',
     linewidth = 0.5
   ) +
   geom_segment(
     data = pro_p_label,
     aes(
       x = seg_x,
       y = against_p_y*0.99,
       xend = seg_xend,
       yend = against_p_y*0.99
     ),
     color = '#0084D7',
     linewidth = 0.5
   ) +
   geom_col(aes(x=year,y=ratio,fill=category), width = 0.8)+
    
    scale_fill_manual(values = c("pro P" = '#8c190f',
                                 "against P" = '#0084D7',
                                 "spoiled ballot" = '#005082'),
                      breaks = c("pro P",
                                 "against P",
                                 "spoiled ballot" ),
                      labels = c("pro P",
                                 "against P",
                                 "spoiled ballot" ))+
    theme_cowplot()+
    theme_minimal_hgrid()+
   # legends
   annotate(
     geom = 'text',
     x = 2020,
     y = pro_p_label$against_p_y[5]*1.26,
     label = "Year-Year difference
including spoiled",
     hjust=0,
     size=3.5,
     color='#0084D7'
   )+
   annotate(
     'curve',
     x = 2020, # Play around with the coordinates until you're satisfied
     y = pro_p_label$against_p_y[5]*1.16,
     yend = pro_p_label$against_p_y[5]*0.99,
     xend = 2021.6,
     linewidth = 0.3,
     color='#0084D7',
     curvature = 0.1,
     arrow = arrow(length = unit(0.1, 'cm'))
   )+
  # pro rus difference  
   geom_label(
     data = pro_p_label,
     aes(x = pro_p_x, y = pro_p_y, label = pro_p_label),
     fill = '#8c190f',
     color = 'white',
     vjust=1,
     size=3.5,
   )+
   # protest difference
   geom_label(
     data = pro_p_label,
     aes(x = against_p_x, y = against_p_y, label = against_p_label),
     fill = '#0084D7',
     color = 'white',
     vjust=1,
     size=3.5
   )+
   # total number
   geom_text(
     data = pro_p_label,
     aes(x = year, y = total, label = total),
     size = 4,
     color = 'grey50',
     vjust=-0.2
   )+
   # ratios
   geom_text(
     data = pro_p_label,
     aes(
       x = year,
       y = mid_pro_y,
       label = paste0(pro_r * 100, "%")
     ),
     angle = 90,
     size = 2.8,
     color = 'grey80'
   ) +
   geom_text(
     data = pro_p_label,
     aes(
       x = year,
       y = mid_against_y,
       label = ifelse(against_r < 0.05, '', paste0(against_r *
                                                     100, "%"))
     ),
     angle = 90,
     size = 2.8,
     color = 'grey80'
   ) +
   geom_text(
     data = pro_p_label,
     aes(
       x = year,
       y = mid_spoiled_y,
       label = ifelse(spoiled_r < 0.05, '', paste0(spoiled_r *
                                                     100, "%"))
     ),
     angle = 90,
     size = 2.8,
     color = 'grey80'
   ) +
   labs(y="people",x="",fill="",
         title=paste0("Russians voted in ",sel_region_label,"  ",sel_country_label))+
    theme(legend.position = 'top',
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid = element_line(color='grey70',linewidth = 0.1))+
          scale_x_continuous(breaks = c(2004,2008,2012,2018,2024))
    
 # g1
 
 g2 <- sum_tmp %>% 
    ggplot()+
    geom_bar(aes(x=year,
                 y=ratio,
                 fill=category), 
                 position = 'fill',
                 stat = 'identity'
                )+
    geom_point(data = pu,
               aes(x=year,y=rat_dec),
               size = 4,
               alpha = 0.9,
               color = 'yellow'
    ) +
    scale_fill_manual(values = c("pro P" = '#8c190f',
                                 "against P" = '#0084D7',
                                 "spoiled ballot" = '#005082'),
                      breaks = c("pro P",
                                 "against P",
                                 "spoiled ballot" ),
                      labels = c("pro P",
                                 "against P",
                                 "spoiled ballot" ))+
    theme_cowplot()+
    theme_minimal_hgrid()+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    labs(y="%",x="",fill="",title=paste0("Russians voted in ",sel_region_label," ",sel_country_label),
         subtitle="Official results, \"Russian\" official results with yellow dot")+
    theme(legend.position = 'top',
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank())+
    scale_x_continuous(breaks = c(2004,2008,2012,2018,2020,2024))
  
if(ratio){ 
  if(plotly_plot){
  ggplotly(g2) %>%  
    plotly::layout(legend=list(x=0, 
                               xanchor='left',
                               yanchor='top',
                               orientation='h'))
    }else{g2}
  
}else{
  if(plotly_plot){ggplotly(g1) %>%  
    plotly::layout(legend=list(x=0, 
                               xanchor='left',
                               yanchor='top',
                               orientation='h'))
}else{g1}}
}

voters_total(ratio=FALSE)
voters_total(sel_country = 'Austria',ratio=FALSE)
voters_total(sel_country = 'Armenia',ratio=FALSE)
voters_total(sel_country = 'Belarus',ratio=FALSE)
voters_total(sel_country = 'United Kingdom',ratio=FALSE)
voters_total(sel_country = 'Croatia',ratio=FALSE)
voters_total(sel_country = 'Iran',ratio=FALSE)
voters_total(sel_country = 'Israel', ratio=FALSE)
voters_total(sel_country = 'Malta',ratio=FALSE)
voters_total(sel_country = 'Germany',ratio=FALSE)
voters_total(sel_region = 'Europe',ratio=FALSE)



# total deltas in out 18-24  ----------------------------------------------
total_voters <- RU04_24 %>% 

  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(year,en_country, alpha.2) %>% 
  summarise(total = sum(number))

gov_voters <- RU04_24 %>% 
  filter(Label %in% c("yes",
                      "Путин Владимир Владимирович",
                      "Медведев Дмитрий Анатольевич"
  )) %>%
  group_by(year,en_country,alpha.2) %>% 
  summarize(gov_total = sum(number))

reject_voters <- RU04_24 %>% 
  filter(Label %in% c("Rejected.ballots",
                      "Число недействительных избирательных бюллетеней")) %>% 
  group_by(year,en_country,alpha.2) %>% 
  summarize(reject_total = sum(number))

against_voters <- RU04_24 %>% 
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
                      "Даванков Владислав Андреевич",
                      "Слуцкий Леонид Эдуардович",
                      "Харитонов Николай Михайлович",
                      "no")) %>% 
  group_by(year,en_country,alpha.2) %>% 
  summarize(against_total = sum(number))


sum_voters <- total_voters %>% 
  left_join(gov_voters) %>% 
  left_join(reject_voters) %>% 
  left_join(against_voters) %>% 
  mutate(gov_ratio = gov_total/total,
         reject_ratio = reject_total/total,
         against_ratio = against_total/total)


# export map data ---------------------------------------------------------


sum_voters %>% 
  filter(year==2008) %>% 
  write.csv('out/df_2008_cntr_results.csv',row.names = FALSE)

ыdf_12_18_people_diff <- sum_voters %>% 
group_by(en_country) %>% 
  arrange(en_country,year) %>% 
  filter(year!=2020) %>% 
  mutate(gov_total_diff =gov_total - lag(gov_total),
         against_total_diff = against_total - lag(against_total),
         reject_total_diff = reject_total - lag(reject_total),
         ag_rej_diff = against_total_diff + reject_total_diff,
         gov_ratio= gov_total/total,
         gov_flag = ifelse(gov_ratio>0.5,"Y","N")) %>% 
  select(year,en_country,gov_total_diff,reject_total,against_total_diff,ag_rej_diff,gov_flag) %>% 
  filter(year==2018) %>% 
  mutate(mymean = mean(c(gov_total_diff,ag_rej_diff))) %>% 
  arrange(mymean) %>% 
  mutate(en_country = factor(en_country,en_country))

threshold <- 600
filter(df_18_24_people_diff, mymean > threshold || mymean < -threshold) %>% 
  write.csv('out/diff_18-24.csv',row.names = FALSE)

g1 <- ggplot(filter(df_18_24_people_diff, mymean > threshold || mymean < -threshold)) +
  geom_segment( aes(x=reorder(en_country,mymean), xend=en_country, y=gov_total_diff, yend=ag_rej_diff,color=gov_flag),
                linewidth=1) +
  geom_point( aes(x=reorder(en_country,mymean), y=gov_total_diff,
                  text=paste0(en_country,'<br>gov votes diff 18-24: ',gov_total_diff)), color='brown', size=3 ) +
  geom_point( aes(x=reorder(en_country,mymean), y=ag_rej_diff,
                  text=paste0(en_country,'<br>anti gov diff 18-24: ',ag_rej_diff)), color='#0084D7', size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('#0084D7','brown'))+
  theme_minimal_vgrid(font_size = 10)+
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Difference in people")+
  labs(title="Difference in votes 2018-2024")

ggplotly(g1,tooltip = 'text')

threshold2 <- 600
filter(df_12_18_people_diff, mymean > threshold || mymean < -threshold) %>% 
  write.csv('out/diff_12-18.csv',row.names = FALSE)

g2 <- ggplot(filter(df_12_18_people_diff, mymean > threshold2 || mymean < -threshold2)) +
  geom_segment( aes(x=reorder(en_country,mymean), xend=en_country, y=gov_total_diff, yend=ag_rej_diff,color=gov_flag),
                linewidth=1) +
  geom_point( aes(x=reorder(en_country,mymean), y=gov_total_diff,
                  text=paste0(en_country,'<br>gov votes diff 12-18: ',gov_total_diff)), color='brown', size=3 ) +
  geom_point( aes(x=reorder(en_country,mymean), y=ag_rej_diff,
                  text=paste0(en_country,'<br>anti gov diff 12-18: ',ag_rej_diff)), color='#0084D7', size=3 ) +
  coord_flip()+
  scale_color_manual(values = c('brown','#0084D7'))+
  theme_minimal_vgrid(font_size = 10)+
  theme(
    legend.position = "none",
  ) +
  xlab("") +
  ylab("Difference in people")+
  labs(title="Difference in votes 2012-2018")

ggplotly(g2,tooltip = 'text')

subplot(g1,g2,nrows = 1) 



# total voters ------------------------------------------------------------

# RU 04_24
total_voters <- function(sel_region = NA, sel_country = NA){
  if(is.na(sel_region)){
    sel_region = all_of(RU04_24$region)
    sel_region_label = NULL
  }  else {
    sel_region
    sel_region_label = sel_region
  }

  if(is.na(sel_country)){
    sel_country = all_of(RU04_24$en_country)
    sel_country_label = NULL
  } else {
    sel_country
    sel_country_label = sel_country
  }
#   
  total_voters <- RU04_24 %>% 
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
    scale_x_continuous(breaks = c(2004,2008,2012,2018,2020, 2024))+
    theme(legend.position = 'top',
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.subtitle = element_text(size=12,color='grey70',hjust=1))
  # g1
  ggplotly(g1)
}
total_voters(sel_country = "Austria")
total_voters(sel_country = "Malta")
# turnout vs result -------------------------------------------------------

# clean up data, leave only PU and ME
RU04_24_PM <-
  RU04_24 %>% 
  filter(str_detect(Label,'Пу|Ме|yes')) %>% 
  group_by(en_country,year,region) %>% 
  summarise(rat = mean(ratio), people = sum(number)) #%>%
  # # filter(region == "Europe") %>%
  # # distinct(en_country) %>% View()
  # mutate(color = case_when(
  #   en_country == "Austria" ~ "red",
  #   en_country == "Netherlands" ~ "forestgreen",
  #   TRUE ~"grey80")
  # )
# write.csv(RU04_24_PM,'Data/RU04_24_per_year.csv',row.names = FALSE)
g1 <- RU04_24_PM %>% 
  # filter(year == 2024) %>% 
  ggplot(aes(x=people,y=rat, 
             color=as.factor(year),
             text = paste0(en_country,'<br>',people,"<br>",rat)))+
  geom_point(alpha = 0.6, size=2)+
  scale_x_log10()+
  theme_minimal()+
  labs(y="процент за Путина, %",x = "кол-во людей, log10()",color="")

ggplotly(g1,tooltip = 'text')

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

# animated plot -------
RU04_24_PM <-
  RU04_24 %>% 
  filter(str_detect(Label,'Пу|Ме|yes')) %>% 
  group_by(en_country,year,region) %>% 
  summarise(rat = mean(ratio), people = sum(number))

g1 <- RU04_24_PM %>%
  mutate(region = ifelse(is.na(region),'Asia',region)) %>% 
  ggplot(aes(x=people,y=rat,fill=region))+
  geom_point(size=2.5,shape=21,alpha=0.7)+
  theme_minimal(base_size = 14)+
  labs(title="How countries voted for Putin",
    subtitle = "year: {frame_time}",
    y = "Putin\'s result , %",
    x = "Amount of people per country, log scale",
    fill ="")+
  labs(fill="")+
  theme(legend.position = 'top')+
  scale_x_log10(labels=scales::label_comma())+
  scale_fill_manual(values = c('#3cab7a','#c71e1d','#fa8c00','#18a1cd','grey'))+
  guides(fill=guide_legend(nrow=1))+
  transition_time(year)+
  ease_aes('linear')

# animate(g1)
animate(g1,
  renderer = gifski_renderer('out/elec_04_24_country.gif'),
  # renderer = av_renderer('Out/test.avi'),
  fps = 5,
  end_pause = 20,
  start_pause = 1,
  nframes = 120,
  height = 400,
  width = 600,
  units = "px")


# people vs % for Putin non animated ---------------------------------------------------

g1 <- RU04_24_PM %>%
  ggplot(aes(x=people,y=rat,color=region,text=paste0(en_country,"<br>Votes: ",people,
                                                     "<br>Putin: ",round(rat),"%")))+
  geom_point(alpha = 0.7, size=2)+
  scale_x_log10()+
  theme_minimal(base_size = 10)+
  panel_border()+
  labs(title = "",
       y = "votes for Putin, %",
       x = "Amount of people, log scale",
       color ="")+
  facet_grid(~year)

ggplotly(g1,tooltip = 'text')
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

exit24_city_split <- exit24_prelim %>% 
  separate(Country,c("ru_country","en_country"),"/") %>% 
  separate(City,c("ru_city","en_city"),"/") %>% 
  fill(en_country) %>% 
  fill(en_city) %>% 
  mutate(en_country = str_trim(en_country)) %>% 
  mutate(ru_city = str_trim(ru_city)) %>% 
  mutate(en_country = case_when(
    en_country == "Great Britain" ~ "United Kingdom",
    en_country == "Federal Republic of Germany" ~ "Germany",
    TRUE ~ en_country)) 
  
  
RU_total_UIK <- RU04_24 %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(UIK,year,en_country,Location) %>% 
  summarise(total = sum(number)) 

ext_uik_list <- exit24_prelim %>% 
  distinct(Voting.station) %>% unlist()

exit_cl <- exit24_city_split %>% 
  select(-ru_country,-en_city) %>% 
  select(en_country, 
         Location = ru_city,
         UIK = Voting.station, 
         Surveyed = Voters.surveyed,
         Exit = Voters.counted.at.the.exit) %>% 
  mutate(total = if_else(is.na(Exit),Surveyed,Exit),
         year = 2024)
  

# putin yes no UIK --------------------------------------------------------


total_voters <- RU04_24 %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(UIK,year) %>% 
  summarise(total = sum(number))



gov_voters <- RU04_24 %>% 
  filter(Label %in% c("yes",
                      "Путин Владимир Владимирович",
                      "Медведев Дмитрий Анатольевич"
  )) %>%
  group_by(UIK,year) %>% 
  summarize(gov_total = sum(number))

reject_voters <- RU04_24 %>% 
  filter(Label %in% c("Rejected.ballots",
                      "Число недействительных избирательных бюллетеней")) %>% 
  group_by(UIK,year) %>% 
  summarize(reject_total = sum(number))

against_voters <- RU04_24 %>% 
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
                      "Даванков Владислав Андреевич",
                      "Слуцкий Леонид Эдуардович",
                      "Харитонов Николай Михайлович",
                      "no")) %>% 
  group_by(UIK,year) %>% 
  summarize(against_total = sum(number))


sum_voters <- total_voters %>% 
  left_join(gov_voters) %>% 
  left_join(reject_voters) %>% 
  left_join(against_voters) %>% 
  mutate(gov_ratio = gov_total/total,
         reject_ratio = reject_total/total,
         against_ratio = against_total/total) %>% 
  mutate(gov_flag = if_else(gov_ratio > 0.5, "Y","N"))

# 2018 vs 2024 --------------------------------------------------------------------

# gov voters difference
rat_dif18_24 <- sum_voters %>% 
  pivot_wider(id_cols = UIK, names_from = year,values_from = gov_ratio) %>% 
  mutate(rat_gov_dif18_24 = round((`2024`-`2018`)*100,0)) %>% 
  filter(!is.na(rat_gov_dif18_24)) %>% 
  select(UIK,rat_gov_dif18_24,`gov_rat_2018` = `2018`,`gov_rat_2024` = `2024`)
  


RU_total_UIK <- RU04_24 %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(UIK,year,en_country,Location) %>% 
  summarise(total = sum(number)) 

df_turnout_18_24 <- 
  RU_total_UIK %>% 
  filter(year %in% c(2018, 2024)) %>% 
  mutate(Location = str_trim(Location)) %>%
  # rows_patch(UIK_country_Loc_24,by=c("UIK",'en_country')) %>% 
  pivot_wider(id_cols = c(UIK,en_country,Location),
              names_from = year,values_from = total) %>% 
  mutate(diff = `2024`- `2018`, ratio = round(diff/`2018`*100,0)) %>% 
  filter(!is.na(`2024`)) %>%
  left_join(filter(sum_voters,year==2024)) %>% 
  mutate(Location = if_else(is.na(Location),paste0(en_country,'-',as.character(UIK)),Location)) %>% 
  arrange(Location) %>% 
  ungroup() %>% 
  mutate(Location_Y = if_else(Location == lag(Location),paste0(Location,'-',as.character(UIK)),Location)) %>% 
  mutate(Location_Y = if_else(is.na(Location_Y),Location,Location_Y))

df_turnout_18_24 %>% 
  filter(!is.na(ratio)) %>% 
  filter(ratio %in% c(min(ratio):-80,80:max(ratio))) %>% 
  arrange(ratio) %>% 
  write.csv('out/df_turnout18-24.csv',row.names = FALSE)


RU18_24_diff <-
RU_total_UIK %>% 
  filter(year %in% c(2018, 2024)) %>% 
  mutate(Location = str_trim(Location)) %>%
  pivot_wider(id_cols = c(UIK,en_country),names_from = year,values_from = total) %>% 
  mutate(tot_diff18_24 = `2024`-`2018`, `tot_rat_dif18_24` = round(tot_diff18_24/`2018`*100,0)) %>% 
  left_join(filter(sum_voters,year==2024),by=join_by("UIK")) %>% 
  left_join(rat_dif18_24,by='UIK') %>% 
  left_join( RU_Pro_flag_upd %>% 
               select(c(UIK,Location,en_country,ru_pro,year)) %>% 
               filter(year==2024),
                by=join_by('UIK',"en_country"))



g1 <- RU18_24_diff %>%
  # filter(year==2024) %>% 
  ggplot(aes(
    x = rat_gov_dif18_24,
    y = tot_rat_dif18_24,
    size = total,
    color=ru_pro,
    text = paste0(
      en_country,
      "<br>УИК: ",UIK,
      "<br>",Location,
      "<br> разница в голосах за П,%: ",
      `rat_gov_dif18_24`,
      "<br>явка, разница в % ",
      tot_rat_dif18_24,
      "<br>общее кол-во: ",
      total
    )
  )) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  labs(y = "явка, разница в % 2018-2024", x = "разница у Путина, 2018-2024 ")

ggplotly(g1, tooltip = 'text')
  

ggplot(aes(x=reorder(Location,-ratio),y=ratio,color=gov_flag))+
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
      ratio,'%',
      '<br> gov ratio: ', round(gov_ratio,2)
  ),
  alpha=0.6,
  ))+
  coord_flip()+
  labs(x="",y="% разницы с 2018 годом",
       title = "Сравнение явки на выборах 2018 и 2024 года, данные ЦИК")+
  theme_minimal()+
  theme(plot.caption = element_text(size=10,hjust=0,vjust=0))
# g1

ggplotly(g1, tooltip = 'text') 


# 2024 UIK vs Ex Poll -----------------------------------------------------


g1 <-
  RU_total_UIK %>% 
  filter(year %in% c(2024)) %>% 
  left_join(exit_cl,by = join_by(UIK)) %>% 
  filter(UIK %in% ext_uik_list) %>%
  rename(total = total.x,ex_poll = total.y,en_country = en_country.x,
         Location = Location.x) %>% 
  mutate( diff = total-ex_poll,
         ratio = round(diff/total*100,0)) %>% 
  filter(!is.na(ratio)) %>% 
  # View()
  ggplot(aes(x=reorder(Location,-ratio),y=ratio))+
  geom_point(aes(
    text = paste0(
      en_country,
      '<br>',
      Location,
      '<br>ЦИК: ',
      total,
      '<br>ExPoll: ',
      ex_poll,
      '<br>',
      ratio,'%'
    )
  ),color='#0084D7')+
  coord_flip()+
  labs(x="",y="% разницы с данными ЦИКа",title = "Сравнение явки на выборах по данным ЦИКа и экзит полов, 2024 года",
       caption = "Явка 2024 показана по результатам экзит пола,
Внимание! Это не всегда полная явка, иногда это кол-во опрошенных, когда нет данных о всех.")+
  theme_minimal()+
  theme(plot.caption = element_text(size=10,hjust=0,vjust=0))
# g1

ggplotly(g1, tooltip = 'text')  




# military -----------------------------------------------------------------


RU_Pro_flag <- RU04_24 %>%
  distinct(UIK,year,Location,country,en_country) %>%
  mutate(Location = str_trim(Location)) %>%
  mutate(ru_pro = case_when(
    str_detect(Location,
               'Миноб|ФСБ|батальон|ОКРМС|штаб|в/ч|Матросский|офицеров|ОШК|ДОФ|КСПМ|Батальон|Клуб 1472')~'Military',
    str_detect(country,'Молда|Абха|Осет')~ "Military",
    UIK == 8027  ~ "Military",
    TRUE ~"Non military"))

RU04_24_PM <-
  RU04_24 %>% 
  filter(str_detect(Label,'Пу|Ме|yes')) %>% 
  group_by(UIK,Location,en_country,year,region) %>% 
  summarise(rat = mean(ratio), people = sum(number)) #%>%

df_mlt <- RU04_24_PM %>% 
  mutate(Location = str_trim(Location)) %>%
  left_join(RU_Pro_flag,by=join_by(UIK,year,Location,en_country)) %>%
  mutate(year = as.factor(year)) %>%
  arrange(year) %>%
  mutate(rat_bin = cut(rat,seq(0,100,by=5),right = TRUE)) %>% 
  group_by(year,rat_bin,ru_pro) %>% 
  summarise(people_bin = sum(people)) 

df_mlt %>% 
  pivot_wider(id_cols = c(rat_bin,year),names_from = ru_pro,values_from = people_bin) %>% 
  mutate(rat_bin = str_replace(rat_bin,'\\(',''),
         rat_bin = str_replace(rat_bin,'\\]',''),
         rat_bin = str_replace(rat_bin,'\\,','-')) %>% 
  filter(year==2024) %>% 
  write.csv('out/df_mlt24.csv',row.names = FALSE)

# histogram ---------------------------------------------------------------
RU04_24_PM <-
  RU04_24 %>% 
  filter(str_detect(Label,'Пу|Ме|yes')) %>% 
  group_by(UIK,Location,en_country,year,region) %>% 
  summarise(rat = mean(ratio), people = sum(number)) #%>%

df_mlt <- RU04_24_PM %>% 
  mutate(Location = str_trim(Location)) %>%
  left_join(RU_Pro_flag,by=join_by(UIK,year,Location,en_country)) #%>%
  mutate(year = as.factor(year)) %>%
  arrange(year) %>%
  mutate(rat_bin = cut(rat,seq(0,100,by=5),right = TRUE)) %>% 
  group_by(year,rat_bin,ru_pro) %>% 
  summarise(people_bin = sum(people)) #%>% 
  pivot_wider(id_cols = c(year,rat_bin),names_from = ru_pro,values_from = people_bin)


g1 <- df_mlt %>% 
  ggplot(aes(x=rat_bin,fill=ru_pro,
             text = paste0("Category:",rat_bin,
                           "<br>People:", people_bin,
                           "<br>",ru_pro)))+
  geom_col(aes(y=people_bin),
           position=position_dodge2(preserve = 'single',padding = 0),width=0.3)+
  scale_fill_manual(values = c('brown','#0084D7'))+
  theme_minimal_hgrid(font_size = 10)+
  facet_grid(year~.)+
  labs(x="процент за Путина, %", y= "голосов за Путина",fill="Военные:")+
  scale_y_continuous(breaks = c(10000,30000,50000,80000),
                     labels = c('10K','30k','50K','80K'))+
   scale_x_discrete(labels = c('10','','20','','30','','40','','50','','60',
                                '','70','','80','','90','','100'))+
  theme(legend.position = 'top',
        axis.line = element_blank(),
        panel.grid.major.y = element_line(color='grey',linewidth=0.01))



ggplotly(g1,tooltip = 'text')

# calc military proportion 
RU04_24_PM %>%
  mutate(Location = str_trim(Location)) %>%
  left_join(RU_Pro_flag,by=join_by(UIK,year,Location,en_country)) %>%
  group_by(year,ru_pro) %>%
  summarise(people = sum(people)) %>%
  pivot_wider(id_cols = year,names_from = ru_pro,values_from = people) %>%
  mutate(Y_N_ratio = `Да`/(`Да`+`Нет`))


RU04_24_PM %>% 
  mutate(Location = str_trim(Location)) %>%
  left_join(RU_Pro_flag,by=join_by(UIK,year,Location,en_country)) %>%
  mutate(rat_bin = cut(rat,seq(0,100,by=5),right = TRUE)) %>% 
  # group_by(year,rat_bin,ru_pro) %>% 
  # summarise(people_bin = sum(people)) %>% 
  filter(year==2018,rat_bin=="(90,95]",ru_pro =="Нет") %>%
  # arrange(rat_bin,-people) %>%
  View()
  

# diff in UIKs between 18-24 ----------------------------------------------


# write.csv('Data/all_UIKs.csv',row.names = FALSE)


RU04_24 %>% 
  filter(year %in% c(2012,2024), en_country=="Israel") %>% 
  distinct(year,UIK) %>% 
  # mutate(UIK = as.character(UIK)) %>%
  group_by(year) %>%
  summarise(xx = n(year))



# misc --------------------------------------------------------------------
RU04_24 %>% 
# RU04_24_PM %>% 
  # filter(year==2008,rat<60) %>% 
  filter(UIK==5131) %>% 
  # filter(en_country == "Cyprus",year%in% c(2018,2024)) %>% 
  # filter(UIK == 8027) %>% 
  # filter(en_country=="Armenia", year==2024, Location=="Ереван") %>% 
  View()
