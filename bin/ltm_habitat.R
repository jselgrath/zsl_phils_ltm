# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  make habitat and meter data in long format

##########################
library(ggplot2)
library(tidyverse)
library(sf)

# ------------------------------------------------------
remove(list=ls())

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/iwao/data/")
# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------
# read lit data 
d1<-read_csv("./handumon/handumon_in/hain_lit/hain_lit_b.csv")%>%
  glimpse()

# meter data
d2<-d1%>%
  select(habd99_met:hab20_met)%>%
  mutate(site_code="HA")%>%
  pivot_longer(!site_code, 
               names_to = "survey", values_to = "meter")%>%
  mutate(survey= str_replace(survey,"_met",""))%>%
  mutate(year1=str_remove_all(survey, "[habdw]"))%>%
  
# first two years in survey code, then numbered by surveys for wet and dry years - for HA
  mutate(year2=if_else(year1==as.numeric(99),1999,as.numeric(year1)))%>%
  mutate(year2=if_else(year2==00,2000,year2))%>%
  mutate(year2=if_else(year2==1|year2==2,2001,year2))%>%
  mutate(year2=if_else(year2==3|year2==4,2002,year2))%>%
  mutate(year2=if_else(year2==5|year2==6,2003,year2))%>%
  mutate(year2=if_else(year2==7|year2==8,2004,year2))%>%
  mutate(year2=if_else(year2==9|year2==10,2005,year2))%>%
  
  mutate(year2=if_else(year2==11|year2==12,2006,year2))%>%
  mutate(year2=if_else(year2==13|year2==14,2007,year2))%>%
  mutate(year2=if_else(year2==14|year2==15,2008,year2))%>%
  mutate(year2=if_else(year2==16|year2==17,2009,year2))%>%
  mutate(year2=if_else(year2==18|year2==19,2010,year2))%>%
  mutate(year2=if_else(year2==20,2011,year2))%>%
  
  # season
  mutate(season=if_else(survey=="habd99","d","tbd"))%>%
  mutate(season=if_else(survey=="habw99","w",season))%>%
  mutate(season=if_else(survey=="habd00","d",season))%>%
  mutate(season=if_else(survey=="habw00","w",season))%>%
  mutate(season=if_else(year1=="1"|year1=="3"|year1=="5"|year1=="7"|year1=="8"|year1=="9"|year1=="11"|year1=="13"|year1=="15"|year1=="17"|year1=="19","d",season))%>%
  mutate(season=if_else(year1=="2"|year1=="4"|year1=="6"|year1=="8"|year1=="10"|year1=="12"|year1=="14"|year1=="16"|year1=="18"|year1=="20","w",season))%>% 
  
  # other things
  mutate(t_code=str_remove_all(survey, "[:digit:]"))%>% # remove number from code
  mutate(i_o_c="O")%>% # for handumon c&d are inside, a&b are outside
  mutate(t_code2=t_code)%>%
  mutate(t_code=str_remove_all(t_code,"[dw]"))%>%
  select(-survey)%>%
  glimpse()

d2
tail(d2)
unique(d2$season)



# habitat data
d3<-d1%>%
  select(habd99_sub:hab20_sub)%>%
  mutate(site_code="HA")%>%
  pivot_longer(!site_code, 
               names_to = "survey", values_to = "habitat")%>%
  mutate(survey= str_replace(survey,"_sub",""))%>%
  mutate(year1=str_remove_all(survey, "[habdw]"))%>%

  # first two years in survey code, then numbered by surveys for wet and dry years - for HA
  mutate(year2=if_else(year1==as.numeric(99),1999,as.numeric(year1)))%>%
  mutate(year2=if_else(year2==00,2000,year2))%>%
  mutate(year2=if_else(year2==1|year2==2,2001,year2))%>%
  mutate(year2=if_else(year2==3|year2==4,2002,year2))%>%
  mutate(year2=if_else(year2==5|year2==6,2003,year2))%>%
  mutate(year2=if_else(year2==7|year2==8,2004,year2))%>%
  mutate(year2=if_else(year2==9|year2==10,2005,year2))%>%
  
  mutate(year2=if_else(year2==11|year2==12,2006,year2))%>%
  mutate(year2=if_else(year2==13|year2==14,2007,year2))%>%
  mutate(year2=if_else(year2==14|year2==15,2008,year2))%>%
  mutate(year2=if_else(year2==16|year2==17,2009,year2))%>%
  mutate(year2=if_else(year2==18|year2==19,2010,year2))%>%
  mutate(year2=if_else(year2==20,2011,year2))%>%
  
  # season
  mutate(season=if_else(survey=="habd99","d","tbd"))%>%
  mutate(season=if_else(survey=="habw99","w",season))%>%
  mutate(season=if_else(survey=="habd00","d",season))%>%
  mutate(season=if_else(survey=="habw00","w",season))%>%
  mutate(season=if_else(year1=="1"|year1=="3"|year1=="5"|year1=="7"|year1=="8"|year1=="9"|year1=="11"|year1=="13"|year1=="15"|year1=="17"|year1=="19","d",season))%>%
  mutate(season=if_else(year1=="2"|year1=="4"|year1=="6"|year1=="8"|year1=="10"|year1=="12"|year1=="14"|year1=="16"|year1=="18"|year1=="20","w",season))%>% 
  
  #other things
  mutate(t_code=str_remove_all(survey, "[:digit:]"))%>%
  mutate(t_code=str_remove_all(t_code,"[dw]"))%>%
  glimpse()

d3
# view(d3)


  

d3a<-d3%>%
  select(survey,habitat)%>% #t_code
  glimpse()
d3a

# merge habitat and meter data
d4<-d2%>%
  cbind(d3a)%>%
  # select(site_code:i_o_c, habitat,t_code2,season)%>%
  select(site_code,meter,year=year2,year1,season,t_code,i_o_c,t_code2,survey,habitat)%>%
  mutate(t_code_l=t_code)%>%
  mutate(t_code=toupper(t_code))%>%#make transect codes uppercase
  glimpse()
d4

unique(d4$survey)

# checking - use this to compare to maps later
d4%>%
  filter(year==2010)%>%
  filter(meter<3)%>%
  glimpse()
  

# 2010 to 2012 (images: 5/10/2010 and 20/4/2012)
d5<-d4%>%
  filter(year>=2010&year==2012)%>%
  arrange(site_code,t_code,year, meter)%>%
  glimpse()

unique(d5$t_code)
  

# save
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")


write_csv(d4,"./results/ha_in_lit_bs_long.csv")
write_csv(d5,"./results/ha_in_lit_bs_long_3yrs.csv")
