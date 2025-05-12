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

d1<-read_csv("./results/ltm_habitat_2010_groups_map_intersect_table.csv")%>%glimpse()
d2<-read_csv("./results/ltm_habitat_2012_groups_map_intersect_table.csv")%>%glimpse()

d5<-read_csv("./results/ltm_habitat_2010_groups_map_closest_table.csv")%>%glimpse()
d6<-read_csv("./results/ltm_habitat_2012_groups_map_closest_table.csv")%>%glimpse()


# intersect, within 0.5m - RS only --------------
# Reef Flat
d1_f<-d1%>%
  filter(Geomorphic=="Reef Flat"&Map=="RS")%>%
  pivot_wider(names_from=group, values_from=n)%>%
  mutate(DC=if_else(is.na(DC),0,DC))%>%
  mutate(LC=if_else(is.na(LC),0,LC))%>%
  mutate(MA=if_else(is.na(MA),0,MA))%>%
  mutate(RUBBLE=if_else(is.na(RUBBLE),0,RUBBLE))%>%
  mutate(SAND=if_else(is.na(SAND),0,SAND))%>%
  mutate(SILT=if_else(is.na(SILT),0,SILT))%>%
  mutate(SASI=SAND+SILT)%>%
  select(-SAND,-SILT,-Map,-Geomorphic)%>%
  group_by(Hab1,Hab_Paper)%>%
  summarize(
    Coral=sum(LC),
    Dead_coral=sum(DC),
    Algae=sum(MA),
    Rubble=sum(RUBBLE),
    SandSilt=sum(SASI),
    Total=Coral+Dead_coral+Algae+Rubble+SandSilt)%>%
  glimpse()
d1_f

# Reef Slope
d1_s<-d1%>%
  filter(Geomorphic=="Reef Slope"&Map=="RS")%>%
  pivot_wider(names_from=group, values_from=n)%>%
  mutate(DC=if_else(is.na(DC),0,DC))%>%
  mutate(LC=if_else(is.na(LC),0,LC))%>%
  mutate(MA=if_else(is.na(MA),0,MA))%>%
  mutate(RUBBLE=if_else(is.na(RUBBLE),0,RUBBLE))%>%
  mutate(SAND=if_else(is.na(SAND),0,SAND))%>%
  mutate(SILT=if_else(is.na(SILT),0,SILT))%>%
  mutate(SASI=SAND+SILT)%>%
  select(-SAND,-SILT,-Map,-Geomorphic,-test_all2)%>%
  group_by(Hab1,Hab_Paper)%>%
  summarize(
    Coral=sum(LC),
    Dead_coral=sum(DC),
    Algae=sum(MA),
    Rubble=sum(RUBBLE),
    SandSilt=sum(SASI),
    Total=Coral+Dead_coral+Algae+Rubble+SandSilt)%>%
  ungroup()%>%
  glimpse()
d1_s





# closest, within 1.5m - RS only --------------
# Reef Flat
d5_f<-d5%>%
  filter(Geomorphic=="Reef Flat"&Map=="RS")%>%
  pivot_wider(names_from=group, values_from=n)%>%
  mutate(DC=if_else(is.na(DC),0,DC))%>%
  mutate(LC=if_else(is.na(LC),0,LC))%>%
  mutate(MA=if_else(is.na(MA),0,MA))%>%
  mutate(RUBBLE=if_else(is.na(RUBBLE),0,RUBBLE))%>%
  mutate(SAND=if_else(is.na(SAND),0,SAND))%>%
  mutate(SILT=if_else(is.na(SILT),0,SILT))%>%
  mutate(SASI=SAND+SILT)%>%
  select(-SAND,-SILT,-Map,-Geomorphic)%>%
  group_by(Hab1,Hab_Paper)%>%
  summarize(
    Coral=sum(LC),
    Dead_coral=sum(DC),
    Algae=sum(MA),
    Rubble=sum(RUBBLE),
    SandSilt=sum(SASI),
    Total=Coral+Dead_coral+Algae+Rubble+SandSilt)%>%
  glimpse()
d5_f

# Reef Slope
d5_s<-d5%>%
  filter(Geomorphic=="Reef Slope"&Map=="RS")%>%
  pivot_wider(names_from=group, values_from=n)%>%
  mutate(DC=if_else(is.na(DC),0,DC))%>%
  mutate(LC=if_else(is.na(LC),0,LC))%>%
  mutate(MA=if_else(is.na(MA),0,MA))%>%
  mutate(RUBBLE=if_else(is.na(RUBBLE),0,RUBBLE))%>%
  mutate(SAND=if_else(is.na(SAND),0,SAND))%>%
  mutate(SILT=if_else(is.na(SILT),0,SILT))%>%
  mutate(SASI=SAND+SILT)%>%
  select(-SAND,-SILT,-Map,-Geomorphic,-test_all2)%>%
  group_by(Hab1,Hab_Paper)%>%
  summarize(
    Coral=sum(LC),
    Dead_coral=sum(DC),
    Algae=sum(MA),
    Rubble=sum(RUBBLE),
    SandSilt=sum(SASI),
    Total=Coral+Dead_coral+Algae+Rubble+SandSilt)%>%
  ungroup()%>%
  glimpse()
d5_s



# save ----------------------------
write_csv(d1_f,"./doc/confmatrix_ltm_2010_rs_flat_intersect.csv")
write_csv(d1_s,"./doc/confmatrix_ltm_2010_rs_slope_intersect.csv")

write_csv(d5_f,"./doc/confmatrix_ltm_2010_rs_flat_close.csv")
write_csv(d5_s,"./doc/confmatrix_ltm_2010_rs_slope_close.csv")
