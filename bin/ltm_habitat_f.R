# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  evaluate accuracy of maps using long term monitoring (ltm) data from the project seahorse foundation (now zsl philippines)
#       clean coordinate data from two sources - brian cabrera (2009 given to Jenny) and 2010 data (check source)

# im not dealing with dry and wet seasons because I am using data from later years, but that should be addressed for 1999 data

##########################
library(ggplot2)
library(tidyverse)
library(sf)


# met = meter
# sub = substrate
# ------------------------------------------------------
remove(list=ls())

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/iwao/data/")
# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------
# transects data Gina uploaded to google drive April 2025

# handumon ----------------
d1<-read_csv("./handumon/handumon_in/hain_lit/hain_lit_b.csv")%>%   glimpse()
d2<-read_csv("./handumon/handumon_in/hain_lit/hain_lit_bs.csv")%>%  glimpse()
d3<-read_csv("./handumon/handumon_in/hain_lit/hain_lit_c.csv")%>%  glimpse()
d4<-read_csv("./handumon/handumon_in/hain_lit/hain_lit_cs.csv")%>%  glimpse()



f1<-function(dta,col_start_m,col_end_m,col_start_h,col_end_h,y1,y2){ # ,col_start_h,col_end_h
  # d1=dta
  # d2=da
  # d3=db
  # d3a=db1
  # d4=dc
  # d5=dd
  
  m_start<-enquo(col_start_m)
  m_end<-enquo(col_end_m)
  h_start<-enquo(col_start_h)
  h_end<-enquo(col_end_h)
    
  # meter data
  da<-dta%>%
    select(!!m_start:!!m_end)%>%
    mutate(site_code="HA")%>%
    pivot_longer(!site_code, 
                 names_to = "survey", values_to = "meter")%>%
    mutate(survey= str_replace(survey,"_met",""))%>%
    mutate(year1=str_remove_all(survey, "[habdw]"))%>%
    mutate(year2=as.numeric(year1)+2000)%>%
    mutate(year2=if_else(year2==2099,1999,year2))%>%
    mutate(t_code=str_remove_all(survey, "[:digit:]"))%>%
    mutate(i_o_c="I")%>%
    mutate(t_code2=t_code)%>%
    mutate(t_code=str_remove_all(t_code,"[dw]"))
  
  # da

# habitat data ----------------------
  db<-dta%>%
    select(!!h_start:!!h_end)%>%
    mutate(site_code="HA")%>%
    pivot_longer(!site_code,
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
  
  # db
  # view(d3)




db1<-db%>%
  select(survey,habitat,t_code)%>%
  glimpse()

# merge habitat and meter data
dc<-da%>%
  cbind(db1)%>%
  select(site_code:i_o_c, habitat,t_code2)%>%
  select(site_code,year=year2,meter,t_code:t_code2,survey)%>%
  mutate(t_code_l=t_code)%>%
  mutate(t_code=toupper(t_code))%>%#make transect codes uppercase
  glimpse()
dc


# checking - use this to compare to maps later
# dc%>%
#   filter(year==2012)%>%
#   filter(meter<3)%>%
#   glimpse()


# 2010 to 2012 (images: 5/10/2010 and 20/4/2012)
dd<-dc%>%
  filter(year==y1|year==y2)%>%
  arrange(site_code,t_code,year, meter)%>%
  glimpse()

unique(dd$t_code)

return(da)

} 




# run function on all datasets ------------------------------------
test<-f1(d1,habd99_met,hab20_met,habd99_sub,hab20_sub,2010,2012)%>%
  glimpse()

# save
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")


write_csv(d4,"./results/ha_in_lit_bs_long.csv")
write_csv(d5,"./results/ha_in_lit_bs_long_2yrs.csv")
