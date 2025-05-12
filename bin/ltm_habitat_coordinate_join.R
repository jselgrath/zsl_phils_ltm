# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  link transect data to habitat data by year

##########################
library(ggplot2)
library(tidyverse)
library(sf)
library(purrr)

# ------------------------------------------------------
remove(list=ls())
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/iwao/data/")
# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")


# habitat data for 2010 to  2012 in long format (probably just till 2011 since changed methods in 2012)
d1<-read_csv("./results/ltm_lit_2010_2011.csv")%>%
  mutate(transect_id=t_code)%>%
  # mutate(distance_m=meter)%>%
  glimpse()

# transect locations (straight line from start to end with points at .1 meter increments)
# fa = in focal area for this project
transect_points<- st_read("./gis/ltm_coordinates_cleaned3_fa.shp")%>%
  mutate(distance_m=round(as.numeric(ORIG_LEN*100000),1),
         transect_id=t_code)%>%
  filter(distance_m<=20)%>%
  select(,-Id,-ORIG_FID,-ORIG_LEN)%>%
  filter(site_code!="JA")%>% # not in cleaned data GB provided in 2025
  glimpse()
transect_points

tail(transect_points)

# plot(transect_points)

# organizing coordinates -----------------------------------------------------------------
# sites with transect coordinates
sites<-transect_points%>%
  tibble()%>%
  select(site_name,site_code)%>%
  unique()%>%
  # filter(site_code!="JA")%>% # not in cleaned data GB provided in 2025
  glimpse()

# transects with coordinates (not sure if this matches habitat files yet)
transects<-transect_points%>%
  tibble()%>%
  select(site_name,site_code,transect_id)%>%
  unique()%>%
  glimpse()
transects
# view(transects)
#  organizing habtiat ---------------------------------------------------------

# select data from sites & transects in focal area with coordinates and data
d3<-transects%>%
  inner_join(d1)%>%
  filter(!is.na(meter))%>%
  glimpse()
# view(d3)



# get mean value for habitat location along transect, so point not a range of distance
d4<-d1%>%
  mutate(distance_m = (meter + lead(meter)) / 2)%>%
  mutate(habitat2=lead(habitat))%>%  # habitat from next line
  filter(!is.na(meter))%>%
  filter(!is.na(habitat2))%>%
  glimpse()

d4

# check
(0+2.52)/2
(2.52+2.94)/2

# subset by year & season so not duplicates
d4_2010<-d4%>%
  filter(year==2010&season=="d")%>%
  glimpse() 

d4_2011<-d4%>%
  filter(year==2011&season=="w")%>%
  glimpse()

 



# join data sets -----------------------------------------
# (prompt in chat gpt: in R how what is the code to join a value to the point that has the closest distance along a transect which the transect has been divided into .1 meter points so is a line of points. the sample points are not a sf and have distance along the transect but do not have spatial coordinates. the data are from multiple transects so there needs to be a group_by variable to make sure that the right values are assigned to the right transect)

# Example: sample_data has columns like "distance_m" and "value" - d4
# transect_points is an sf object with a "distance_m" column - d2

# Step 1: Do a rowwise join of sample_data to closest transect point *by group*

# for 2010 -----------------------
samples<-d4_2010
  # group_by(season,t_code2,survey,i_o_c)


# Function to assign closest habitat within each transect
assign_closest_habitats <- function(transect_points, samples) {
  transect_points %>%
    group_by(transect_id) %>%
    nest() %>%
    left_join(samples %>% group_by(transect_id) %>% nest(), by = "transect_id") %>%
    mutate(
      data = map2(data.x, data.y, ~{
        df_points <- .x
        df_samples <- .y
        if (nrow(df_samples) == 0) {
          df_points$habitat <- NA
          return(df_points)
        }
        df_points$habitat <- sapply(df_points$distance_m, function(d) {
          i <- which.min(abs(df_samples$distance_m - d))
          df_samples$habitat[i]
        })
        df_points
      })
    ) %>%
    select(transect_id, data) %>%
    unnest(cols = data)
}

# run function ----------------------------------
result_2010 <- assign_closest_habitats(transect_points, d4_2010)%>%
  filter(!is.na(habitat))%>%
  ungroup()%>%
  glimpse()

result_2011 <- assign_closest_habitats(transect_points, d4_2011)%>%
  filter(!is.na(habitat))%>%
  ungroup()%>%
  glimpse()

hab<-result_2010%>%
  select(habitat)%>%
  unique()%>%
  arrange(habitat)%>%
  glimpse()


# make spatial -------------------
result_2010b<-st_as_sf(result_2010)%>%
  glimpse()

result_2011b<-st_as_sf(result_2011)%>%
  glimpse()
  
 plot(result_2011b[1]) 




# save
st_write(result_2010b,"./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_2010", append=FALSE)
st_write(result_2011b,"./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_2012", append=FALSE)

write_csv(hab,"./results/habitats_2010.csv")
