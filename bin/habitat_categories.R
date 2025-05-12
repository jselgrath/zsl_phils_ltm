# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  link transect data to habitat data by year

##########################
library(ggplot2)
library(tidyverse)
library(sf)


# ------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/iwao/data/")
# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")


d1<-read_csv("./results/habitats_2010.csv")%>%
  glimpse()

d2<-read_csv("./data/ltm_habitat_codes/BenthicCategories.csv")%>%
  mutate(habitat=Category)%>%
  select(habitat,Description,Group)%>%
  mutate(group=if_else(habitat=="TA","MA",Group),
         group=if_else(habitat=="SC","LC",group),
         group=if_else(habitat=="GORG","LC",group),
         group=if_else(habitat=="RCK","RCK",group),
         group=if_else(habitat=="AA","MA",group),
         group=if_else(habitat=="SG","SG",group),
         group=if_else(habitat=="ZO","LC",group))%>%
  select(-Group,-Description)%>%
  unique()%>%
  glimpse()

# view(d2)

d3<-d1%>%
  left_join(d2)%>%
  unique()%>%
  arrange(group)%>%
  glimpse()

d3

write_csv(d3,"./results/habitat_categories.csv")
