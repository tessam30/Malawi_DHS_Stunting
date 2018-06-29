# Create the infrastructure for the projects

pacman::p_load("purrr", "tidyverse", "llamar", "svywrangler",
               "RColorBrewer", "Hmisc", "here", 
               "viridis", "haven", "readxl", "magrittr",
               "sf", "gridExtra")
#test again
# -- Run once at setup Create datain and data out folders
  dir.create("Dataout")
  dir.create("Datain")
  dir.create("GIS")
  dir.create("Plots")
  
# -- Where are we starting from? Setting paths where data has been migrated to
# Data paths --------------------------------------------------------------


here()
dhs2010 <- 'Datain/MW_2010_DHS'
dhs2015 <- 'Datain/MW_2015-16_DHS'
out_dir <- 'Dataout'
gis_dir <- 'GIS'

# Syntax to load files from DHS folders; either works
# file.path(here(), dhs2010, "MWBR61DT", "MWBR61FL.DTA")
# read_dta(here(dhs2010, "MWBR61DT", "MWBR61FL.DTA"))

source('dhs_helpers.R')
source('attributes.R')
