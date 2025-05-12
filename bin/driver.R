# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal:  driver file to process potential ground truthing files

# initials: 
# AN - Angie Nellas, former ZSL Philippines biologist
# BC - Brian Cabrera

# acronyms: 
# lit - line intersect transect
# pq - photoquadrats
# fa = focal area for Jennifer Selgrath's PhD - a 19km x 22km area in the central Danajon Bank, Visayas, Philippines

# a,b,c,d - transects 
# s = shallow

#--------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(sf)

# ------------------------------------------------------
remove(list=ls())


# ---------------------------------------------------------------------------------------------
# TRY ALLAN CORAL REEF ATLAS GROUNDTRUTHING DATA -------
#-----------------------------------------------------------
setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# try to use Allan coral reef atlas for ground truthing
# this has low agreement, but is based on manual(?) demarking satellite images, and is from a different year. Decided not to use because too much uncertainty.
source(("./bin/allan_cra_test.R"))
# input: ./data/random_pt_allan_phd_20250416.csv
# output: ./doc/random_pt_sum.csv


#---------------------------------------------------------------------------------------------
# PROJECT LTM LIT COORDINATES USED IN 2009 FOR GROUNDTRUTHING -------
#-----------------------------------------------------------

# projecting start and end point files of transects from work with Andy Nelson (at IRRI)
source ("./bin/ltm_coordinates_shp_2009_export.R")
# input:    ./gis/phd_psf_ltm/ltm_transects_gis/LTM_data_AndyNelson_Dec09/export1.shp
#           ./gis/phd_psf_ltm/ltm_transects_gis/LTM_data_AndyNelson_Dec09/Export3.shp
# output:   ./gis/phd_psf_ltm/ltm_transects_gis/ltm_locations_2009.gpkg", layer="start"
#           ./gis/phd_psf_ltm/ltm_transects_gis/ltm_locations_2009.gpkg", layer="end"

# ---------------------------------------------------------------------------------------------
# CLEAN AND PROJECT PSF/ ZSL PHILIPPINES LTM LIT DATA FROM ALL YEARS FROM DATA IWAO FUJII CLEANED DURING HIS MSc -------
#-----------------------------------------------------------

setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/ltm")


# clean transect names and coordinates ---------------------------------------------------
# link gps coordinates collected by PS in ~2010 with full transect names from AN
source("./bin/ltm_coordinates_clean.R")
# input:  ./data/ltm_coordinates/ltm_transects_updated_AN_20100928.csv  # full transect names from AN. # AN sat down and made a full list for reference. other lists I have found are not complete
#         ./data/ltm_coordinates/ltm_transect_coordinates_gps_20101206_cleaned.csv # coordinates from ~ 2010
#         ./data/ltm_coordinates/ltm_transect_coordinates_20091202.csv # coordinates Jenny had from BC in 2009 that were used to georeference some transects when she was at IRRI (their GIS head helped her)
# output: ./results/ltm_coordinates_cleaned.csv  - all transects, some with missing coordinates
#         ./results/pq_coordinates_cleaned.csv   - photquad
#         ./results/ltm_coordinates_cleaned2.csv - only transects with full coordinates

# this output is put into ArcPro to make georeferenced lines and points along the transect line



# organize lit data: make habitat and meter data in long format ----------------------------------------
# this is just for one transect to build the base of the function
source("./bin/ltm_habitat.R")
# input:    ./handumon/handumon_in/hain_lit/hain_lit_b.csv # lit data
# output:   ./results/ha_in_lit_bs_long.csv
#           ./results/ha_in_lit_bs_long_3yrs.csv          # only 2010-2012 data


# organize lit data: make habitat and meter data in long format ----------------------------------------
# same as above but for all LIT data in focal area - note Jagoliao is in the focal area, but not in the data gina gave me in 2025. not worrying about it for now...
# this is a function that is run across all lit survey data in the focal area
source("./bin/ltm_habitat.R")
# input:    ./handumon/handumon_in/hain_lit/hain_lit_X.csv
#           ./handumon/handumon_out/haout_lit/hain_lit_X.csv
#           ./jandayan_norte/jandayan_norte_in/...
#           ./jandayan_norte/jandayan_norte_out/...
#           ./pandanon/pandanon_in/...
#           ./pandanon/pandanon_out/...
#           ./putik_cs/putik_in/...
# output:   ./results/ltm_lit_all_long.csv
#           ./results/ltm_lit_2010_2011.csv


# join long LIT data with geographiccoordinates to the nearest 0.01 m
source("./bin/ltm_habitat_coordinate_join.R")
# input:    ./results/ha_in_lit_bs_long_3yrs.csv
#           ./gis/ltm_coordinates_cleaned3_fa.shp # this is coordinates of transects projected in ArcPro and turned into points at 0.01m
# output:   ./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_2010"
#          ./gis/ltm_habitat_spatial/ltm_habitat.gpkg", layer="ltm_habitat_2012"
#          ./results/habitats_2010.csv

# join habitat categories from 2010 with codes from psf
source("./bin/habitat_categories.R")
# input:   ./results/habitats_2010.csv
#          ./data/ltm_habitat_codes/BenthicCategories.csv 
# output:  ./results/habitat_categories.csv


# join codes and groups with spatial point data
source("./bin/ltm_habitat_coordinate_join_cat.R")
# input:   ./results/habitat_categories.csv
#          ./gis/ltm_habitat_spatial/ltm_habitat.gpkg, layer="ltm_habitat_2010"
#          ./gis/ltm_habitat_spatial/ltm_habitat.gpkg, layer="ltm_habitat_2012"
# output:  ./gis/ltm_habitat_spatial/ltm_habitat.gpkg, layer="ltm_habitat_2010_groups"
#          ./gis/ltm_habitat_spatial/ltm_habitat.gpkg, layer="ltm_habitat_2012_groups"
