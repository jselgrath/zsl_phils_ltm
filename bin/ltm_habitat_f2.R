# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  function to process turning lit data into long format for multiple


##########################
library(ggplot2)
library(tidyverse)
library(sf)


# met = meter
# sub = substrate

# jagoliao is also is focal area, but data is not in folders Iwao cleaned

# ------------------------------------------------------
remove(list=ls())

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/iwao/data/")
# setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------
# transects data Gina uploaded to google drive April 2025
# need to change start and stop years when run fucntion based on years surveyed for each transect

# handumon in ----------------
d1<-read_csv("./handumon/handumon_in/hain_lit/hain_lit_b.csv")%>%   glimpse()
d2<-read_csv("./handumon/handumon_in/hain_lit/hain_lit_bs.csv")%>%  glimpse()
d3<-read_csv("./handumon/handumon_in/hain_lit/hain_lit_c.csv")%>%  glimpse()
d4<-read_csv("./handumon/handumon_in/hain_lit/hain_lit_cs.csv")%>%  glimpse()

# handumon out ----------------
d5<-read_csv("./handumon/handumon_out/haout_lit/haout_lit_a.csv")%>%   glimpse()
d6<-read_csv("./handumon/handumon_out/haout_lit/haout_lit_as.csv")%>%  glimpse()
d7<-read_csv("./handumon/handumon_out/haout_lit/haout_lit_d.csv")%>%  glimpse()
d8<-read_csv("./handumon/handumon_out/haout_lit/haout_lit_ds.csv")%>%  glimpse()

# putik - control site
d9<-read_csv("./putik_cs/pu_lit/pu_lit_a.csv")%>%   glimpse()
d10<-read_csv("./putik_cs/pu_lit/pu_lit_as.csv")%>%   glimpse()
d11<-read_csv("./putik_cs/pu_lit/pu_lit_b.csv")%>%   glimpse()
d12<-read_csv("./putik_cs/pu_lit/pu_lit_bs.csv")%>%   glimpse()
  
# pandanon in ----------------
d13<-read_csv("./pandanon/pandanon_in/pain_lit/pain_lit_a.csv")%>%   glimpse()
d14<-read_csv("./pandanon/pandanon_in/pain_lit/pain_lit_as.csv")%>%  glimpse()
d15<-read_csv("./pandanon/pandanon_in/pain_lit/pain_lit_b.csv")%>%  
  select(-pabw98_met,-pabd98_sub)%>% #error - one says wet and one says dry. not using these years, so not looking into this for now
  glimpse()
d16<-read_csv("./pandanon/pandanon_in/pain_lit/pain_lit_bs.csv")%>%  glimpse()

# pandanon out ----------------
d17<-read_csv("./pandanon/pandanon_out/paout_lit/paout_lit_c.csv")%>%   glimpse()
d18<-read_csv("./pandanon/pandanon_out/paout_lit/paout_lit_cs.csv")%>%  glimpse()
d19<-read_csv("./pandanon/pandanon_out/paout_lit/paout_lit_ds.csv")%>%  glimpse()

# jandayan norte in ----------------
d21<-read_csv("./jandayan_norte/jandayan_norte_in/jnin_lit/jnin_lit_a.csv")%>%   glimpse()
d22<-read_csv("./jandayan_norte/jandayan_norte_in/jnin_lit/jnin_lit_as.csv")%>%  glimpse()
d23<-read_csv("./jandayan_norte/jandayan_norte_in/jnin_lit/jnin_lit_b.csv")%>%  glimpse()
d24<-read_csv("./jandayan_norte/jandayan_norte_in/jnin_lit/jnin_lit_bs.csv")%>%  glimpse()

# jandayan norte out ----------------
d25<-read_csv("./jandayan_norte/jandayan_norte_out/jnout_lit/jnout_lit_c.csv")%>%   glimpse()
d26<-read_csv("./jandayan_norte/jandayan_norte_out/jnout_lit/jnout_lit_cs.csv")%>%  glimpse()
d27<-read_csv("./jandayan_norte/jandayan_norte_out/jnout_lit/jnout_lit_d.csv")%>%  glimpse()
d28<-read_csv("./jandayan_norte/jandayan_norte_out/jnout_lit/jnout_lit_ds.csv")%>%  glimpse()

# asinan gh ----------------
d31<-read_csv("./asinan/asinan_gh/bvgh_lit/bvgh_lit_a.csv")%>%   glimpse()
d32<-read_csv("./asinan/asinan_gh/bvgh_lit/bvgh_lit_as.csv")%>%  glimpse()
d33<-read_csv("./asinan/asinan_gh/bvgh_lit/bvgh_lit_b.csv")%>%  glimpse()
d34<-read_csv("./asinan/asinan_gh/bvgh_lit/bvgh_lit_bs.csv")%>%  glimpse()

# asinan post ----------------
d35<-read_csv("./asinan/asinan_post/bvpost_lit/bvpost_lit_c.csv")%>%   glimpse()
d36<-read_csv("./asinan/asinan_post/bvpost_lit/bvpost_lit_cs.csv")%>%  glimpse()
d37<-read_csv("./asinan/asinan_post/bvpost_lit/bvpost_lit_d.csv")%>%  glimpse()
d38<-read_csv("./asinan/asinan_post/bvpost_lit/bvpost_lit_ds.csv")%>%  glimpse()

# ---------------------------------------------------------------------------------------------------
# FUNCTION TO CHANGE DATA TO LONG FORMAT -----------------------------------------------------------
# ---------------------------------------------------------------------------------------------------

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
    mutate(type="habitat")%>%
    pivot_longer(!type,
                 names_to = "survey", values_to = "meter")%>%
    mutate(survey= str_replace(survey,"_met",""))%>%
    mutate(year1=str_remove_all(survey, "[habcdwpuvsjn]"))%>%
    
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
    mutate(season=if_else(year1=="99","tbd","tbd"))%>% # different naming system so need to 99 and 00 separately outside of fxn
    mutate(season=if_else(year1=="00","tbd","tbd"))%>%
    mutate(season=if_else(year1=="1"|year1=="3"|year1=="5"|year1=="7"|year1=="8"|year1=="9"|year1=="11"|year1=="13"|year1=="15"|year1=="17"|year1=="19","d",season))%>%
    mutate(season=if_else(year1=="2"|year1=="4"|year1=="6"|year1=="8"|year1=="10"|year1=="12"|year1=="14"|year1=="16"|year1=="18"|year1=="20","w",season))%>% 
    
    # other things
    mutate(t_code=str_remove_all(survey, "[:digit:]"))%>%
    mutate(t_code2=t_code)%>%
    mutate(t_code=str_remove_all(t_code,"[dw]"))%>%
    select(-survey)
  
  # da

# habitat data ----------------------
  db<-dta%>%
    select(!!h_start:!!h_end)%>%
    mutate(type="habitat")%>%
    pivot_longer(!type,
                 names_to = "survey", values_to = "habitat")%>%
    mutate(survey= str_replace(survey,"_sub",""))%>%
    mutate(year1=str_remove_all(survey, "[habcdwpuvsjn]"))%>%
    
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
    mutate(season=if_else(year1=="99","tbd","tbd"))%>% # different naming system so need to 99 and 00 separately outside of fxn
    mutate(season=if_else(year1=="00","tbd","tbd"))%>%
    mutate(season=if_else(year1=="1"|year1=="3"|year1=="5"|year1=="7"|year1=="8"|year1=="9"|year1=="11"|year1=="13"|year1=="15"|year1=="17"|year1=="19","d",season))%>%
    mutate(season=if_else(year1=="2"|year1=="4"|year1=="6"|year1=="8"|year1=="10"|year1=="12"|year1=="14"|year1=="16"|year1=="18"|year1=="20","w",season))%>% 
    
    # other things
    mutate(year2=as.numeric(year1)+2000)%>%
    mutate(year2=if_else(year2==2099,1999,year2))%>%
    mutate(t_code=str_remove_all(survey, "[:digit:]"))%>%
    # mutate(i_o_c="I")%>%
    # mutate(t_code2=t_code)%>%
    mutate(t_code=str_remove_all(t_code,"[dw]"))

  # db
  # view(d3)




db1<-db%>%
  select(survey,habitat)#,t_code)
#
# # merge habitat and meter data
dc<-da%>%
  cbind(db1)%>%
  # select(site_code:i_o_c, habitat,t_code2,season)%>%
  select(year=year2,year1,season,t_code,t_code2,meter,habitat,survey)%>%
  mutate(habitat=if_else(meter==0.00,lead(habitat),habitat))%>% # set 0 to same habitat as first transition
  mutate(t_code_l=t_code)%>%
  mutate(t_code3=toupper(t_code))%>%#make transect codes uppercase
  mutate(t_code=toupper(t_code2))
# glimpse()
# # dc
# 
# 
# # checking - use this to compare to maps later
# # dc%>%
# #   filter(year==2012)%>%
# #   filter(meter<3)%>%
# #   glimpse()
# 
# 
# 2010 to 2012 (images: 5/10/2010 and 20/4/2012)
dd<-dc%>%
  # filter(year>=y1&year<=y2)%>%
  filter(meter<=20)%>%
  arrange(t_code2,year, meter)

# unique(dd$t_code)

return(dd)

} 



# ----------------------------------------------------------------------------------------------
# run function on all datasets ------------------------------------
# ----------------------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------------------
# HANDUMON  -------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# Handumon INSIDE -------------------------------------------------
# hab ------------
d1a<-f1(d1,habd99_met,hab20_met,habd99_sub,hab20_sub,2010,2012)%>%
  mutate(site_code="HA")%>%
  mutate(season=if_else(survey=="habw99"|survey=="habw00","w",season))%>%
  mutate(season=if_else(survey=="habd99"|survey=="habd00","d",season))%>%
  mutate(i_o_c="I")%>%
  # mutate(habitat  =if_else(meter==0.00,lead(habitat),habitat))%>% # set 0 to same habitat as first transition
  glimpse()
d1a

range(d1a$year)
range(d1a$meter)

# habs -----------
d2a<-f1(d2,habsw99_met,habs20_met,habsw99_sub,habs20_sub,2010,2012)%>%  
  mutate(site_code="HA")%>%
  mutate(season=if_else(survey=="habsw99"|survey=="habsw00","w",season))%>%
  mutate(season=if_else(survey=="habsd99"|survey=="habsd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()

d2a

# hac ------------
d3a<-f1(d3,hacd99_met,hac20_met,hacd99_sub,hac20_sub,2010,2012)%>%
  mutate(site_code="HA")%>%
  mutate(season=if_else(survey=="hacw99"|survey=="hacw99","w",season))%>%
  mutate(season=if_else(survey=="hacd99"|survey=="hacd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d3a

# hacs ------------
d4a<-f1(d4,hacsw99_met,hacs20_met,hacsw99_sub,hacs20_sub,2010,2012)%>% # no dry 1999
  mutate(site_code="HA")%>%
  mutate(season=if_else(survey=="hacsw99"|survey=="hacsw99","w",season))%>%
  mutate(season=if_else(survey=="hacsd99"|survey=="hacsd00","d",season))%>% 
  mutate(i_o_c="I")%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()
d4a

# join handumon in
ha_in<-rbind(d1a,d2a,d3a,d4a)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()




# Handumon OUTSIDE -------------------------------------------------
# haa ------------
d5a<-f1(d5,haad99_met,haa20_met,haad99_sub,haa20_sub,2009,2011)%>%
  mutate(site_code="HA")%>%
  mutate(season=if_else(survey=="habw99"|survey=="habw00","w",season))%>%
  mutate(season=if_else(survey=="habd99"|survey=="habd00","d",season))%>%
  mutate(i_o_c="O")%>%
  glimpse()
d5a

# haas ------------
d6a<-f1(d6,haasw99_met,haas20_met,haasw99_sub,haas20_sub,2009,2011)%>% 
  mutate(site_code="HA")%>%
  mutate(season=if_else(survey=="habw99"|survey=="habw00","w",season))%>%
  mutate(season=if_else(survey=="habd99"|survey=="habd00","d",season))%>%
  mutate(i_o_c="O")%>%
  glimpse()
d6a

# had ------------
d7a<-f1(d7,hadw99_met,had20_met,hadw99_sub,had20_sub,2009,2011)%>%
  mutate(site_code="HA")%>%
  mutate(season=if_else(survey=="habw99"|survey=="habw00","w",season))%>%
  mutate(season=if_else(survey=="habd99"|survey=="habd00","d",season))%>%
  mutate(i_o_c="O")%>%
  glimpse()
d7a

# hads ------------
d8a<-f1(d8,hadsw99_met,hads20_met,hadsw99_sub,hads20_sub,2009,2011)%>%
  mutate(site_code="HA")%>%
  mutate(season=if_else(survey=="habw99"|survey=="habw00","w",season))%>%
  mutate(season=if_else(survey=="habd99"|survey=="habd00","d",season))%>%
  mutate(i_o_c="O")%>%
  glimpse()
d8a


# join handumon out
ha_out<-rbind(d5a,d6a,d7a,d8a)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()

# ----------------------------------------------------------------------------------------------
# PUTIK - control site --------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# pu a  ------------
d9a<-f1(d9,puaw99_met,pua20_met,puaw99_sub,pua20_sub,2010,2012)%>%
  mutate(site_code="PU")%>%
  mutate(season=if_else(survey=="puaw99"|survey=="puaw00","w",season))%>%
  mutate(season=if_else(survey=="puad99"|survey=="puad00","d",season))%>%
  mutate(i_o_c="C")%>%
  glimpse()
d9a

range(d9a$year)
range(d9a$meter)

# pu as  ------------
d10a<-f1(d10,puasw99_met,puas20_met,puasw99_sub,puas20_sub,2010,2012)%>%
  mutate(site_code="PU")%>%
  mutate(season=if_else(survey=="puasw99"|survey=="puasw00","w",season))%>%
  mutate(season=if_else(survey=="puasd99"|survey=="puasd00","d",season))%>%
  mutate(i_o_c="C")%>%
  glimpse()
d10a

# pu b -----------
d11a<-f1(d11,pubw99_met,pub20_met,pubw99_sub,pub20_sub,2010,2012)%>%  
  mutate(site_code="PU")%>%
  mutate(season=if_else(survey=="pubw99"|survey=="pubw00","w",season))%>%
  mutate(season=if_else(survey=="pubd99"|survey=="pubd00","d",season))%>%
  mutate(i_o_c="C")%>%
  glimpse()

d11a

# pu bs -----------
d12a<-f1(d12,pubsw99_met,pubs20_met,pubsw99_sub,pubs20_sub,2010,2012)%>%  
  mutate(site_code="PU")%>%
  mutate(season=if_else(survey=="pubsw99"|survey=="pubsw00","w",season))%>%
  mutate(season=if_else(survey=="pubsd99"|survey=="pubsd00","d",season))%>%
  mutate(i_o_c="C")%>%
  glimpse()

d12a


# join putik
pu<-rbind(d9a,d10a,d11a,d12a)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()





# ----------------------------------------------------------------------------------------------
# PANDANON --------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# Pandanon INSIDE -------------------------------------------------
# pa a ------------
d13a<-f1(d13,paaw98_met,paa20_met,paaw98_sub,paa20_sub,2010,2012)%>%
  mutate(site_code="PA")%>%
  mutate(season=if_else(survey=="paaw98"|survey=="paaw99"|survey=="paaw00","w",season))%>%
  mutate(season=if_else(survey=="paad98"|survey=="paad99"|survey=="paad00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d1a

range(d13a$year)
range(d13a$meter)


# pa as -----------
d14a<-f1(d14,paasw99_met,paas20_met,paasw99_sub,paas20_sub,2010,2012)%>%  
  mutate(site_code="PA")%>%
  mutate(season=if_else(survey=="paasw99"|survey=="paasw00","w",season))%>%
  mutate(season=if_else(survey=="paasd99"|survey=="paasd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d14a

# pa b -------------
d15a<-f1(d15,pabd99_met,pab20_met,pabd99_sub,pab20_sub,2010,2012)%>%
  mutate(site_code="PA")%>%
  mutate(season=if_else(survey=="pabw98"|survey=="pabw99"|survey=="pabw00","w",season))%>%
  mutate(season=if_else(survey=="pabd98"|survey=="pabd99"|survey=="pabd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d15a


# pa bs -----------
d16a<-f1(d16,pabsw99_met,pabs20_met,pabsw99_sub,pabs20_sub,2010,2012)%>%  
  mutate(site_code="PA")%>%
  mutate(season=if_else(survey=="pabsw99"|survey=="pabsw00","w",season))%>%
  mutate(season=if_else(survey=="pabsd99"|survey=="pabsd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d16a

# join pandanon in
pa_in<-rbind(d13a,d14a,d15a,d16a)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()




# Pandanon OUTSIDE -------------------------------------------------
# pa c ------------
d17a<-f1(d17,pacw99_met,pac20_met,pacw99_sub,pac20_sub,2009,2011)%>%
  mutate(site_code="PA")%>%
  mutate(season=if_else(survey=="pacw99"|survey=="pacw00","w",season))%>%
  mutate(season=if_else(survey=="pacd99"|survey=="pacd00","d",season))%>%
  mutate(i_o_c="O")%>%
  glimpse()
d17a

# pa cs ------------
d18a<-f1(d18,pacsw99_met,pacs20_met,pacsw99_sub,pacs20_sub,2009,2011)%>% 
  mutate(site_code="PA")%>%
  mutate(season=if_else(survey=="pacsw99"|survey=="pacsw00","w",season))%>%
  mutate(season=if_else(survey=="pacsd99"|survey=="pacsd00","d",season))%>%
  mutate(i_o_c="O")%>%
  glimpse()
d18a

# pa ds ------------
d19a<-f1(d19,padsw99_met,pads20_met,padsw99_sub,pads20_sub,2009,2011)%>%
  mutate(site_code="PA")%>%
  mutate(season=if_else(survey=="padsw99"|survey=="padsw00","w",season))%>%
  mutate(season=if_else(survey=="padsd99"|survey=="padsd00","d",season))%>%
  mutate(i_o_c="O")%>%
  glimpse()
d8a


# join pandanon out
pa_out<-rbind(d17a,d18a,d19a)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()





# ----------------------------------------------------------------------------------------------
# JANDAYAN NORTE  -------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# Jandayan Norte INSIDE -------------------------------------------------
# jn a ------------
d21a<-f1(d21,jna5_met,jna20_met,jna5_sub,jna20_sub,2010,2012)%>%
  mutate(site_code="JN")%>%
  mutate(i_o_c="I")%>%
  glimpse()
d21a

range(d21a$year)
range(d21a$meter)

# jn as -----------
d22a<-f1(d22,jnas5_met,jnas20_met,jnas5_sub,jnas20_sub,2010,2012)%>%  
  mutate(site_code="JN")%>%
  mutate(i_o_c="I")%>%
  glimpse()
d22a

# jn b ------------
d23a<-f1(d23,jnb5_met,jnb20_met,jnb5_sub,jnb20_sub,2010,2012)%>%
  mutate(site_code="JN")%>%
  mutate(i_o_c="I")%>%
  glimpse()
d23a

# jn bs ------------
d24a<-f1(d24,jnbs5_met,jnbs20_met,jnbs5_sub,jnbs20_sub,2010,2012)%>% # no dry 1999
  mutate(site_code="JN")%>%
  mutate(i_o_c="I")%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()
d24a

# join jandayan norte in
jn_in<-rbind(d21a,d22a,d23a,d24a)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()




# jandayan norte OUTSIDE -------------------------------------------------
# jn c ------------
d25a<-f1(d25,jnc6_met,jnc20_met,jnc6_sub,jnc20_sub,2009,2011)%>%
  mutate(site_code="JN")%>%
  mutate(i_o_c="O")%>%
  glimpse()
d25a

# jn cs ------------
d26a<-f1(d26,jncs6_met,jncs20_met,jncs6_sub,jncs20_sub,2009,2011)%>% 
  mutate(site_code="JN")%>%
  mutate(i_o_c="O")%>%
  glimpse()
d26a

# jn d ------------
d27a<-f1(d27,jnd5_met,jnd20_met,jnd5_sub,jnd20_sub,2009,2011)%>%
  mutate(site_code="JN")%>%
  mutate(i_o_c="O")%>%
  glimpse()
d27a

# jn ds ------------
d28a<-f1(d28,jnds5_met,jnds20_met,jnds5_sub,jnds20_sub,2009,2011)%>%
  mutate(site_code="JN")%>%
  mutate(i_o_c="O")%>%
  glimpse()
d28a


# join jandayan norte out
jn_out<-rbind(d25a,d26a,d27a,d28a)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()


# ----------------------------------------------------------------------------------------------
# ASINAN --------------------------------------------------------
# ----------------------------------------------------------------------------------------------

# ASINAN guardhouse  (BV = buena vista) -------------------------------------------------
# bvgh_lit_a ------------
d31a<-f1(d31,bvaw98_met,bva20_met,bvaw98_sub,bva20_sub,2010,2012)%>%
  mutate(site_code="BV")%>%
  mutate(season=if_else(survey=="bvaw98"|survey=="bvaw99"|survey=="bvaw00","w",season))%>%
  mutate(season=if_else(survey=="bvad98"|survey=="bvad99"|survey=="bvad00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d31a

range(d31a$year)
range(d31a$meter)


# bv as -----------
d32a<-f1(d32,bvasw99_met,bvas20_met,bvasw99_sub,bvas20_sub,2010,2012)%>%  
  mutate(site_code="BV")%>%
  mutate(season=if_else(survey=="bvasw99"|survey=="bvasw00","w",season))%>%
  mutate(season=if_else(survey=="bvasd99"|survey=="bvasd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d32a

# bv b -------------
d33a<-f1(d33,bvbd99_met,bvb20_met,bvbd99_sub,bvb20_sub,2010,2012)%>%
  mutate(site_code="BV")%>%
  mutate(season=if_else(survey=="bvbw98"|survey=="bvbw99"|survey=="bvbw00","w",season))%>%
  mutate(season=if_else(survey=="bvbd98"|survey=="bvbd99"|survey=="bvbd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d33a


# bv bs -----------
d34a<-f1(d34,bvbsw99_met,bvbs20_met,bvbsw99_sub,bvbs20_sub,2010,2012)%>%  
  mutate(site_code="BV")%>%
  mutate(season=if_else(survey=="bvbsw99"|survey=="bvbsw00","w",season))%>%
  mutate(season=if_else(survey=="bvbsd99"|survey=="bvbsd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d34a

# join asinan/bv guardhouse
bv_gh<-rbind(d31a,d32a,d33a,d34a)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()




# ASINAN C and D are also technically INSIDE -------------------------------------------------
# bv c ------------
d35a<-f1(d35,bvcw98_met,bvcw98_met,bvcw98_sub,bvcw98_sub,2009,2011)%>%
  mutate(site_code="BV")%>%
  mutate(season=if_else(survey=="bvcw98"|survey=="bvcw99"|survey=="bvcw00","w",season))%>%
  mutate(season=if_else(survey=="bvcd98"|survey=="bvcd99"|survey=="bvcd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d35a

# bv cs ------------
d36a<-f1(d36,bvcsw99_met,bvcs20_met,bvcsw99_sub,bvcs20_sub,2009,2011)%>% 
  mutate(site_code="BV")%>%
  mutate(season=if_else(survey=="bvcsw99"|survey=="bvcsw00","w",season))%>%
  mutate(season=if_else(survey=="bvcsd99"|survey=="bvcsd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d36a

# bv d ------------
d37a<-f1(d37,bvdw99_met,bvd20_met,bvdw99_sub,bvd20_sub,2009,2011)%>%
  mutate(site_code="BV")%>%
  mutate(season=if_else(survey=="bvdsw99"|survey=="bvdsw00","w",season))%>%
  mutate(season=if_else(survey=="bvdsd99"|survey=="bvdsd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d37a

# bv ds ------------
d38a<-f1(d38,bvdsw99_met,bvds20_met,bvdsw99_sub,bvds20_sub,2009,2011)%>%
  mutate(site_code="BV")%>%
  mutate(season=if_else(survey=="bvdsw99"|survey=="bvdsw00","w",season))%>%
  mutate(season=if_else(survey=="bvdsd99"|survey=="bvdsd00","d",season))%>%
  mutate(i_o_c="I")%>%
  glimpse()
d38a


# join ASINAN post
bv_post<-rbind(d35a,d36a,d37a,d38a)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()




# ----------------------------------------------------------------------------------------------
# join all ------------------------------------------
# ----------------------------------------------------------------------------------------------
d_all<-rbind(ha_in,ha_out,pu,pa_in,pa_out,jn_in,jn_out,bv_gh,bv_post)%>%
  arrange(year,season,t_code,meter)%>%
  glimpse()


# filter for years of satellite images
d_yr<-d_all%>%
  filter(year>=2010)%>%
  glimpse()

range(d_yr$year)
range(d_yr$meter)

# ----------------------------------------------------------------------------------------------
# save -----------------------------------------------------
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")

write_csv(d_all,"./results/ltm_lit_all_long.csv")
write_csv(d_yr,"./results/ltm_lit_2010_2011.csv")
