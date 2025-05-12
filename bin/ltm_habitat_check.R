# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  evaluate accuracy of maps with ltm data

##########################
library(ggplot2)
library(tidyverse)
library(sf)

# ------------------------------------------------------
remove(list=ls())

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")
# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------

d1<-read_csv("./results/ltm_habitat_2010_groups_map_intersect.csv")%>% # 0.5 m buffer
  filter(!is.na(group))%>%
  filter(group!="OTHER")%>%
  filter(Hab_Paper!="Deep")%>%
  glimpse()
d2<-read_csv("./results/ltm_habitat_2012_groups_map_intersect.csv")%>% # 0.5 m buffer
  filter(!is.na(group))%>%
  filter(group!="OTHER")%>%
  filter(Hab_Paper!="Deep")%>%
  glimpse()

d3<-read_csv("./results/ltm_habitat_2010_groups_map_1_5m.csv")%>%
  filter(!is.na(group))%>%
  filter(group!="OTHER")%>%
  filter(Hab_Paper!="Deep")%>%
  glimpse()
d4<-read_csv("./results/ltm_habitat_2012_groups_map_1_5m.csv")%>%
  filter(!is.na(group))%>%
  filter(group!="OTHER")%>%
  filter(Hab_Paper!="Deep")%>%
  glimpse()

d5<-read_csv("./results/ltm_habitat_2010_groups_map_1_5m_closest.csv")%>%
  filter(!is.na(group))%>%
  filter(group!="OTHER")%>%
  filter(Hab_Paper!="Deep")%>%
  glimpse()
d6<-read_csv("./results/ltm_habitat_2012_groups_map_1_5m_closest.csv")%>%
  filter(!is.na(group))%>%
  filter(Hab_Paper!="Deep")%>%
  glimpse()

unique(d5$group)
unique(d5$Hab_Paper)



# ------------------------------
# compare ------------------
# ------------------------------

# 2010 - intersect -----------------------
d1a<-d1%>%
  mutate(test_c=if_else(group=="LC"&Hab_Paper=="Coral",1,0),
         test_r=if_else(group=="RUBBLE"&Hab_Paper=="Rubble",1,0),
         test_r=if_else(group=="DC"&Hab_Paper=="Rubble",1,test_r),
         test_s=if_else((group=="SAND" |group== "SILT") &Hab_Paper=="Sand",1,0))%>%
  mutate(
    test_all=test_c+test_r+test_s,
    test_all2=ifelse(test_all==0&Hab_Paper=="Deep",NA,test_all))%>%
  glimpse()

# summarize
d1b<-d1a%>%
  group_by(test_all2,group,Hab1,Hab_Paper,Map,Geomorphic)%>% 
  summarize(
    n=length(test_all2))%>%
  glimpse()
d1b

view(d1b)

# 2012 - intersect -----------------------
d2a<-d2%>%
  mutate(test_c=if_else(group=="LC"&Hab_Paper=="Coral",1,0),
         test_r=if_else(group=="RUBBLE"&Hab_Paper=="Rubble",1,0),
         test_r=if_else(group=="DC"&Hab_Paper=="Rubble",1,test_r),
         test_s=if_else((group=="SAND" |group== "SILT") &Hab_Paper=="Sand",1,0))%>%
  mutate(
    test_all=test_c+test_r+test_s,
    test_all2=ifelse(test_all==0&Hab_Paper=="Deep",NA,test_all))%>%
  glimpse()

# summarize
d2b<-d2a%>%
  group_by(test_all2,group,Hab1,Hab_Paper,Map,Geomorphic)%>% 
  summarize(
    n=length(test_all2))%>%
  glimpse()
d2b

view(d2b)




# 2010 - closest -----------------------
d5a<-d5%>%
  mutate(test_c=if_else(group=="LC"&Hab_Paper=="Coral",1,0),
         test_r=if_else(group=="RUBBLE"&Hab_Paper=="Rubble",1,0),
         test_r=if_else(group=="DC"&Hab_Paper=="Rubble",1,test_r),
         test_s=if_else((group=="SAND" |group== "SILT") &Hab_Paper=="Sand",1,0))%>%
  mutate(
         test_all=test_c+test_r+test_s,
         test_all2=ifelse(test_all==0&Hab_Paper=="Deep",NA,test_all))%>%
  glimpse()

# summarize
d5b<-d5a%>%
  group_by(test_all2,group,Hab1,Hab_Paper,Map,Geomorphic)%>% 
  summarize(
    n=length(test_all2))%>%
  glimpse()
d5b

view(d5b)



# 2012 - closest -----------------------
d6a<-d6%>%
  mutate(test_c=if_else(group=="LC"&Hab_Paper=="Coral",1,0),
         test_r=if_else(group=="RUBBLE"&Hab_Paper=="Rubble",1,0),
         test_r=if_else(group=="DC"&Hab_Paper=="Rubble",1,test_r),
         test_s=if_else((group=="SAND" |group== "SILT") &Hab_Paper=="Sand",1,0))%>%
  mutate(
    test_all=test_c+test_r+test_s,
    test_all2=ifelse(test_all==0&Hab_Paper=="Deep",NA,test_all))%>%
  glimpse()

# summarize
d6b<-d6a%>%
  group_by(test_all2,group,Hab1,Hab_Paper,Map,Geomorphic)%>% 
  summarize(
    n=length(test_all2))%>%
  glimpse()
d6b

view(d6b)


# save --------------------------
write_csv(d1b,"./results/ltm_habitat_2010_groups_map_intersect_table.csv")
write_csv(d2b,"./results/ltm_habitat_2012_groups_map_intersect_table.csv")

write_csv(d5b,"./results/ltm_habitat_2010_groups_map_closest_table.csv")
write_csv(d6b,"./results/ltm_habitat_2012_groups_map_closest_table.csv")