library(tidyverse)
library(cowplot)
library(plotly)
library("gganimate")
library('gifski')


# import data -------------------------------------------------------------


RU04_24 <- read.csv('Data/RU04_24_Russia.csv')

# calc voters proportions per country and year ----------------------------


total_voters_cntr <- RU04_24 %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(year,en_country, alpha.2) %>% 
  summarise(total = sum(number))

gov_voters_cntr <- RU04_24 %>% 
  filter(Label %in% c("yes",
                      "Путин Владимир Владимирович",
                      "Медведев Дмитрий Анатольевич"
  )) %>%
  group_by(year,en_country,alpha.2) %>% 
  summarize(gov_total = sum(number))

reject_voters_cntr <- RU04_24 %>% 
  filter(Label %in% c("Rejected.ballots",
                      "Число недействительных избирательных бюллетеней")) %>% 
  group_by(year,en_country,alpha.2) %>% 
  summarize(reject_total = sum(number))

against_voters_cntr <- RU04_24 %>% 
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


sum_voters_cntr <- total_voters_cntr %>% 
  left_join(gov_voters_cntr) %>% 
  left_join(reject_voters_cntr) %>% 
  left_join(against_voters_cntr) %>% 
  mutate(gov_ratio = gov_total/total,
         reject_ratio = reject_total/total,
         against_ratio = against_total/total,
         ag_rej_ratio = 1-gov_ratio,
         ag_rej_total = against_total + reject_total)


# add military voters details ----
df_mlt_total_cntr <- RU04_24 %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(year,en_country, alpha.2,ru_pro) %>% 
  summarise(mil_total = sum(number)) %>% 
  filter(ru_pro=='Military') %>% select(-ru_pro)

# add mlt total to sum of voters
sum_voters_cntr_tmp <- left_join(sum_voters_cntr,df_mlt_total_cntr) %>% 
  mutate(mil_ratio = mil_total/total)



# merge Abkhazia and Osetia -----------------------------------------------
df_AbOs <- sum_voters_cntr_tmp %>% 
  filter(en_country %in% c("Osetia","Abkhazia")) %>% 
  select(year,total,gov_total,reject_total,against_total) %>% 
  group_by(year) %>% 
  summarise(total = sum(total),
            gov_total = sum(gov_total),
            reject_total = sum(reject_total),
            against_total = sum(against_total)) %>% 
  mutate(en_country = "Abkhazia & Osetia",
         alpha.2 = "GE",
         ag_rej_total = (against_total + reject_total),
         gov_ratio = gov_total/total,
         reject_ratio = reject_total/total,
         against_ratio = against_total/total,
         ag_rej_ratio = (against_total + reject_total)/total,
         mil_total = total,
         mil_ratio =1) 

sum_voters_cntr_2 <- sum_voters_cntr_tmp %>% 
  filter(!en_country %in% c("Osetia","Abkhazia")) %>% 
  bind_rows(df_AbOs)
# calc diff to previous year
# - take 2024
# - take 2018
# calc diff gov_total, ag+rej total
# add to 2024 data
# year1 = 2024
# year2 = 2018
diff_years <- function(year1,year2){

tmp1 <- sum_voters_cntr_2 %>% 
  select(year,en_country,alpha.2,total,gov_total,ag_rej_total) %>% 
  filter(year==year1) 
  
  
tmp2 <- sum_voters_cntr_2 %>% 
  select(year,en_country,alpha.2,total,gov_total,ag_rej_total) %>% 
  filter(year==year2)

year1_year2_total = paste0('_total_diff')
year1_year2_gov_total = paste0('_gov_total_diff')
year1_year2_ag_rej_total = paste0('_ag_rej_total_diff')
year2_gov_ratio = paste0("prev_gov_rat")


year1_year2_total_rat = paste0('_total_diff_rat')
year1_year2_gov_total_rat = paste0('_gov_total_diff_rat')
year1_year2_ag_rej_total_rat = paste0('_ag_rej_total_diff_rat')

tmp3 <- left_join(tmp1,tmp2,by=join_by(en_country,alpha.2)) %>% 
  mutate({{year1_year2_total}} :=  total.x-total.y,
         {{year1_year2_gov_total}} :=  gov_total.x-gov_total.y,
         {{year1_year2_ag_rej_total}} :=  ag_rej_total.x-ag_rej_total.y,
         {{year2_gov_ratio}} := gov_total.y/total.y,
         prev_ag_rej_ratio = 1-prev_gov_rat,
         {{year1_year2_total_rat}} := get(!!year1_year2_total)/total.y,
         {{year1_year2_gov_total_rat}} := get(!!year1_year2_gov_total)/total.y,
         {{year1_year2_ag_rej_total_rat}} := get(!!year1_year2_ag_rej_total)/total.y) %>% 
  select(year=year.x,en_country,alpha.2,year1_year2_total,year1_year2_gov_total,
         year1_year2_ag_rej_total,year1_year2_total_rat,year1_year2_gov_total_rat,
         year1_year2_ag_rej_total_rat,year2_gov_ratio,prev_ag_rej_ratio)

}

df_18_24 <- diff_years(2024,2018)
df_18_20 <- diff_years(2020,2018)
df_12_18 <- diff_years(2018,2012)
df_08_12 <- diff_years(2012,2008)
df_04_08 <- diff_years(2008,2004)

# add difference per year
df_for_map <- left_join(sum_voters_cntr_2,df_18_24,by=join_by("year","en_country","alpha.2")) %>% 
  rows_patch(df_12_18,by=c("year","en_country","alpha.2")) %>% 
  rows_patch(df_08_12,by=c("year","en_country","alpha.2")) %>% 
  rows_patch(df_04_08,by=c("year","en_country","alpha.2")) %>% 
  rows_patch(df_18_20,by=c("year","en_country","alpha.2")) 
  
# export four dataframes
for (i in c(2004,2008,2012,2020,2018,2024)){
df_for_map %>% 
  ungroup() %>% 
  mutate(across(contains('rat'),~ round(.x*100,0))) %>% 
  filter(year == i) %>% 
  write.csv(paste0('out/df_map_',i,'.csv'),row.names = FALSE)}


# per region plus minus for and against -----------------------------------

df_all <- df_for_map %>% 
  select(year,en_country,alpha.2,`_gov_total_diff`,`_ag_rej_total_diff`) %>% 
  filter(year != 2004) %>% 
  group_by(year) %>% 
  summarise(gov_diff = sum(`_gov_total_diff`,na.rm=TRUE),ag_rej_diff = sum(`_ag_rej_total_diff`,na.rm=TRUE)) %>% 
  mutate(region = "All abroad")

cntr <- read.csv('Data/all_countries.csv',sep = ";") %>% select(alpha.2,region,sub.region)

df_reg <- df_for_map %>% 
  select(year,en_country,alpha.2,`_gov_total_diff`,`_ag_rej_total_diff`) %>% 
  filter(year != 2004) %>% 
  left_join(cntr) %>% 
  group_by(year,region) %>% 
  summarise(gov_diff = sum(`_gov_total_diff`,na.rm=TRUE),ag_rej_diff = sum(`_ag_rej_total_diff`,na.rm=TRUE)) 

df_diff_per_years <- bind_rows(df_all,df_reg) %>% 
  mutate(year_label = case_when(
    year==2008 ~ '\'04-\'08',
    year==2012 ~ '\'08-\'12',
    year==2018 ~ '\'12-\'18',
    year==2020 ~ '\'18-\'20',
    year==2024 ~ '\'20-\'24'
  ))
df_diff_per_years %>% 
  filter(!region %in% c("Africa","Oceania")) %>%
  write.csv('out/df_diff_per_years_region.csv',row.names = FALSE)
# export crossplot turnout 18-24 ------------------------------------------------

df_18_24 %>% 
  select(en_country,alpha.2,`_total_diff_rat`,`_total_diff`) %>% 
  filter( `_total_diff_rat` > 0.5 || `_total_diff_rat`< -0.5) %>% 
  filter(`_total_diff` > 100 || `_total_diff` < -100) %>%
  arrange(-`_total_diff_rat`) %>%
  # View()
write.csv('out/df_turnout_diff.csv',row.names = FALSE)
# cumsum 2024 -------------------------------------------------------------

df_cum_sum_cntr <- df_for_map %>% 
  filter(year==2024) %>% 
  select(en_country,alpha.2,votes = total,mil_ratio,gov_ratio) %>% 
  arrange(-votes) %>% 
  rowid_to_column(.,'index') %>% 
  left_join(total_voters_all_years) %>% 
  ungroup() %>% 
  mutate(tot_ratio = votes/total,cum_ratio = cumsum(tot_ratio)) 

df_cum_sum_cntr %>% 
  filter(cum_ratio < 0.71) %>% 
  select(en_country,alpha.2,tot_ratio,cum_ratio) %>%
  # View()
  write.csv('Out/df_cumsum_cntr.csv',row.names = FALSE)


# overall military proportion ---------------------------------------------

RU04_24_PM <-
  RU04_24 %>% 
  filter(str_detect(Label,'Пу|Ме|yes')) %>% 
  group_by(UIK,Location,en_country,year,region,ru_pro) %>% 
  summarise(rat = mean(ratio), people = sum(number))

total_voters_all_years <- RU04_24 %>% 
  filter(Label %in% c("Число недействительных избирательных бюллетеней",
                      "Число действительных избирательных бюллетеней","Ballots.in.box")) %>% 
  group_by(year) %>% 
  summarise(total = sum(number))

df_mlt_all_years <-  RU04_24_PM %>%
  mutate(Location = str_trim(Location)) %>%
  group_by(year,ru_pro) %>%
  summarise(people = sum(people)) %>%
  pivot_wider(id_cols = year,names_from = ru_pro,values_from = people) %>%
  mutate(Y_N_ratio = `Military`/(`Military`+`Non military`)) %>% 
  mutate(PM_total = `Military`+`Non military`) %>% 
  left_join(total_voters_all_years) %>% 
  select(year,pm_military = `Military`,pm_non_military = `Non military`,total) %>% 
  mutate(PM_ratio = (pm_military+pm_non_military)/total,
         against_pm =total - (pm_military+pm_non_military))

df_upd <- df_mlt_all_years %>% 
  filter(year==2024) %>% 
  mutate(fake_ballots = 36838,
         pm_non_military = pm_non_military-fake_ballots) %>% 
  select(year,fake_ballots,pm_non_military)

df_mlt_all_years_upd <- left_join(df_mlt_all_years,df_upd,by=c('year')) %>% 
  mutate(pm_non_military = ifelse(is.na(pm_non_military.y),pm_non_military.x,pm_non_military.y)) %>% 
  select(year,pm_military,pm_non_military,against_pm,fake_ballots)

df_mlt_all_years_upd %>% 
  write.csv('out/df_mlt_all_years.csv',row.names = FALSE)


# military voters per year output -----------------------------------------

# df_mlt <- RU04_24_PM %>% 
#   mutate(Location = str_trim(Location)) %>%
#   # left_join(RU_Pro_flag,by=join_by(UIK,year,Location,en_country)) %>%
#   mutate(year = as.factor(year)) %>%
#   arrange(year) %>%
#   mutate(rat_bin = cut(rat,seq(0,100,by=5),right = TRUE)) %>% 
#   group_by(year,rat_bin,ru_pro) %>% 
#   summarise(people_bin = sum(people)) 
# 
# df_mlt %>% 
#   pivot_wider(id_cols = c(rat_bin,year),names_from = ru_pro,values_from = people_bin) %>% 
#   mutate(rat_bin = str_replace(rat_bin,'\\(',''),
#          rat_bin = str_replace(rat_bin,'\\]',''),
#          rat_bin = str_replace(rat_bin,'\\,','-')) %>% 
#   filter(year==2018) %>% 
#   write.csv('Out/df_mlt24.csv',row.names = FALSE)
# update military flag ----------------------------------------------------

RU_Pro_flag <- RU04_24 %>%
  distinct(UIK,year,Location,country,en_country) %>%
  mutate(Location = str_trim(Location)) %>%
  mutate(ru_pro = case_when(
    str_detect(Location,
               'Миноб|ФСБ|батальон|ОКРМС|штаб|в/ч|Матросский|офицеров|ОШК|ДОФ|КСПМ|Батальон|Клуб 1472')~'Military',
    str_detect(country,'Молда|Абха|Осет')~ "Military",
    UIK %in% c(8027,8032,8033,8034,8131,8132,8269)  ~ "Military",
    TRUE ~"Non military"))

# names(RU_Pro_flag)

# RU04_24_upd <- rows_update(RU04_24,RU_Pro_flag,by=c('UIK','year','Location','country','en_country'))

# RU04_24_upd %>% 
#   filter(year==2018) %>% 
#   distinct(UIK,en_country,country,Location,alpha.2,ru_pro) %>% 
#   View()

# write.csv(RU04_24_upd,'Data/RU04_24_Russia.csv',row.names = FALSE)


# misc --------------------------------------------------------------------


RU04_24%>% 
  filter(year==2024, en_country=="Canada") %>% 
  # arrange() %>% 
  View()
