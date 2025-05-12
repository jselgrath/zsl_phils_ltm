# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  evaluate accuracy of maps using long term monitoring (ltm) data from the project seahorse foundation (now zsl philippines)

##########################
library(ggplot2)
library(tidyverse)
library(sf)

# ------------------------------------------------------
remove(list=ls())

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")

# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------
# full transect names from Angie Nellas
d1<-read_csv("./data/ltm_coordinates/ltm_transects_updated_AN_20100928.csv")%>%
  glimpse()




#  coordinates from 2010---------------------------
d4<-read_csv("./data/ltm_coordinates/ltm_transect_coordinates_gps_20101206_cleaned.csv")%>%
  arrange(site_name)%>%
  glimpse()

# join with Angie's full list of sites
d4b<-d1%>%
  full_join(d4)%>%
  arrange(t_code)%>%
  filter(!is.na(E))%>%
  filter(pq!=1)%>%
  glimpse()
# 
# view(d4)

# pq only
d4c<-d4b%>%
  full_join(d4)%>%
  arrange(t_code)%>%
  filter(!is.na(E))%>%
  filter(pq==1)%>%
  glimpse()



# coordinates from IRRI transects - coordinates used for  ground truthing for older data ---------------------
d5<-read_csv("./data/ltm_coordinates/ltm_transect_coordinates_20091202.csv")%>%
  glimpse()



# merge 
d6<-d5%>%
  full_join(d4b)%>%
  mutate(e2=if_else(!is.na(E),E,e))%>%
  mutate(n2=if_else(!is.na(N),N,n))%>%
  mutate(year2=if_else(!is.na(E),"2019",year))%>%
  mutate(source2=if_else(!is.na(E),"new",source))%>%
  select(t_code,municipality,site_name:pair_number,e=e2,n=n2,year2,transect_code_m,note=location,note2=notes2)%>%
  select(-person,-municipality)%>%
  mutate(i_o_c=if_else(site_code=="CS","C",i_o_c))%>%
  mutate(deep_shallow=if_else(t_code=="CS2AS","S",deep_shallow))%>%
  mutate(deep_shallow=if_else(t_code=="CS2A","D",deep_shallow))%>%
  arrange(t_code)%>%
  glimpse()

# view(d6)

# join with Angie's full list of sites
d7<-d1%>%
  full_join(d6)%>%
  arrange(t_code)%>%
  filter(!is.na(e))%>%
  mutate(municipality=if_else(site_code=="CS","Tubigon",municipality))%>% # fill in missing municipality names
  mutate(municipality=if_else(site_code=="BA","Tubigon",municipality))%>%
  mutate(municipality=if_else(site_code=="BB","Tubigon",municipality))%>%
  glimpse()

d7

view(d7)

# pivot wider to make start and end coordinates in one row
d8<- d7%>%
  select(-transect_code_m)%>%
  pivot_wider(names_from = meter, values_from = c(e,n))%>%
  glimpse()
d8

view(d8)

# only transects with complete coordinates
d9<-d8%>%
  filter(!is.na(e_0)&!is.na(n_0)&!is.na(e_50)&!is.na(n_50))%>%
  glimpse()

view(d9)

# save
write_csv(d7,"./results/ltm_coordinates_cleaned.csv")
write_csv(d4c,"./results/pq_coordinates_cleaned.csv")
write_csv(d9,"./results/ltm_coordinates_cleaned2.csv")

# this output is put into ArcPro to make georeferenced lines and points along the transect line
