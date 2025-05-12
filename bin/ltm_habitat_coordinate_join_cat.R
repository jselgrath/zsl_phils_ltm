# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  join habiat codes to spaitally referenced transect data

##########################
library(ggplot2)
library(tidyverse)
library(sf)

# ------------------------------------------------------
remove(list=ls())

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")
# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------
# read lit and habtiat data
d1<-st_read("./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_2010")%>%   glimpse()
d2<-st_read("./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_2012")%>%   glimpse()
d3<-read_csv("./results/habitat_categories.csv")%>% glimpse()

# join habitat to 2010 data
d1a<-d1%>%
  left_join(d3)%>%
  glimpse()

# join habiatat to 2011 data
d2a<-d2%>%
  left_join(d3)%>%
  glimpse()


# save
st_write(d1a,"./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_2010_groups", append=FALSE)
st_write(d2a,"./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_2012_groups", append=FALSE)