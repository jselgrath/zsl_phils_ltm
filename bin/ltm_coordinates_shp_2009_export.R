# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  evaluate accuracy of maps using long term monitoring (ltm) data from the project seahorse foundation (now zsl philippines)
# projecting start and end point files of transects from work with Andy Nelson (at IRRI)

##########################
library(ggplot2)
library(tidyverse)
library(sf)
library(foreign)

# ------------------------------------------------------
remove(list=ls())

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/iwao")

# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------
# start points - from data avaliable in 2009 only
d1<-st_read("./gis/phd_psf_ltm/ltm_transects_gis/LTM_data_AndyNelson_Dec09/export1.shp")%>% #LTM_habitat sections_7Jan10.shp")%>%
  glimpse()

d1

# end points
d2<-st_read("./gis/phd_psf_ltm/ltm_transects_gis/LTM_data_AndyNelson_Dec09/Export3.shp")%>% #LTM_habitat sections_7Jan10.shp")%>%
  glimpse()

plot(d1)
plot(d2)


# save
st_write(d1,"./gis/phd_psf_ltm/ltm_transects_gis/ltm_locations_2009.gpkg", layer="start", overwrite=T)
st_write(d2,"./gis/phd_psf_ltm/ltm_transects_gis/ltm_locations_2009.gpkg", layer="end", overwrite=T)
