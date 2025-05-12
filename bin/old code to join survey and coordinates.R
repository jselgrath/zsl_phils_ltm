
# ---------------------------------------

matched <- dta %>%
  rowwise() %>%
  mutate(
    nearest_idx = which.min(
      abs(d2$distance_m[d2$t_code == t_code] - distance_m)
    ),
    transect_row = which(d2$t_code == t_code)[nearest_idx]
  ) %>%
  ungroup()

# Step 2: Join the geometry from transect_points using the matched index
d5 <- d2[matched$transect_row, ] %>%
  bind_cols(matched %>% select(-nearest_idx, -transect_row))%>%
  glimpse()

d5

# organize output ---------------------------------------------------------------
#2010
d6<-d5%>%
  select(year,site_name,site_id=site_code...4,transect_id=t_code...2,i_o_c=i_o_c...5,deep_shallow=deep_shall,distance_m=distance_m...18,distance_m_actual=distance_m...29,habitat=habitat2,municipality=municipali,notes,note,note2,notes3,pair_number=pair_numbe,geometry)%>%
  glimpse()
d6


# for 2012 -----------------------------------------
dta<-d4_2012

matched2 <- dta %>%
  rowwise() %>%
  mutate(
    nearest_idx = which.min(
      abs(d2$distance_m[d2$t_code == t_code] - distance_m)
    ),
    transect_row = which(d2$t_code == t_code)[nearest_idx]
  ) %>%
  ungroup()

# Step 2: Join the geometry from transect_points using the matched index
d7 <- d2[matched2$transect_row, ] %>%
  bind_cols(matched2 %>% select(-nearest_idx, -transect_row))%>%
  glimpse()

d7

# organize output ---------------------------------------------------------------
#2010
d8<-d7%>%
  select(year,site_name,site_id=site_code...4,transect_id=t_code...2,i_o_c=i_o_c...5,deep_shallow=deep_shall,distance_m=distance_m...18,distance_m_actual=distance_m...29,habitat=habitat2,municipality=municipali,notes,note,note2,notes3,pair_number=pair_numbe,geometry)%>%
  glimpse()
d8

# save
st_write(d6,"./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_ha_2010", overwrite=T)
st_write(d8,"./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_ha_2012", overwrite=T)
