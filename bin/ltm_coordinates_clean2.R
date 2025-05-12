# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  evaluate accuracy of maps using long term monitoring (ltm) data from the project seahorse foundation (now zsl philippines)
#       

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
  mutate(site="HA")%>%
  pivot_longer(!site, 
               names_to = "survey", values_to = "meter")%>%
  mutate(survey= str_replace(survey,"_met",""))%>%
  mutate(year1=str_remove_all(survey, "[habdw]"))%>%
  mutate(year2=as.numeric(year1)+2000)%>%
  mutate(year2=if_else(year2==2099,1999,year2))%>%
  mutate(t_code=str_remove_all(survey, "[:digit:]"))%>%
  mutate(i_o_c="I")%>%
  mutate(t_code2=t_code)%>%
  mutate(t_code=str_remove_all(t_code,"[dw]"))%>%
  glimpse()

d2

# habitat data
d3<-d1%>%
  select(habd99_sub:hab20_sub)%>%
  mutate(site="HA")%>%
  pivot_longer(!site, 
               names_to = "survey", values_to = "habitat")%>%
  mutate(survey= str_replace(survey,"_sub",""))%>%
  mutate(year1=str_remove_all(survey, "[habdw]"))%>%
  mutate(year2=as.numeric(year1)+2000)%>%
  mutate(year2=if_else(year2==2099,1999,year2))%>%
  mutate(t_code=str_remove_all(survey, "[:digit:]"))%>%
  # mutate(i_o_c="I")%>%
  # mutate(t_code2=t_code)%>%
  mutate(t_code=str_remove_all(t_code,"[dw]"))%>%
  glimpse()

d3
view(d3)

d3a<-d3%>%
  select(survey,habitat,t_code)%>%
  glimpse()

# merge
d4<-d2%>%
  cbind(d3a)%>%
  select(site:i_o_c, habitat,t_code2)%>%
  select(site,year=year2,meter,t_code:t_code2,survey)%>%
  glimpse()
d4

# 2009 to 2012
d5<-d4%>%
  filter(year>=2010&year<=2012)%>%
  arrange(site,t_code,year, meter)%>%
  glimpse()
  

# save
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")


write_csv(d4,"./results/ha_in_lit_bs_long.csv")
write_csv(d4,"./results/ha_in_lit_bs_long_3yrs.csv")
