# Use sf to join points to polygons in R
# library(tidyverse)
# library(sf)
# library(here)

# sf doesn't seem to like concatenated fields, tabso we will change working directory
#setwd(str_c(gis_dir, "mwi_2015_shp/shps/"))


# 2015 Shapefile ----------------------------------------------------------
# Using the here package to avoid all this concatenating and path switching
setwd(here("GIS/mwi_2015_shp/shps"))

# Load the shapefile with the attributes you want to attach to the lat/lon files
mwi_poly_2015 <- st_read(dsn = '.', layer = 'sdr_subnational_boundaries2')

setwd(here())
setwd(file.path(here(), dhs2015, "MWGE7AFL"))
      
mwi_points_2015 <- st_read(dsn = '.', layer = 'MWGE7AFL')

# Join the attribution information from the shapefile to the pointfile.
mwi_cluster_2015 <- st_intersection(mwi_points_2015, mwi_poly_2015)

mwi_cluster_2015 = mwi_cluster_2015 %>%
  mutate(dhsregion = (str_to_title(DHSREGEN)),
         coordflag = ifelse(LATNUM != 0, 0, 1))

label(mwi_cluster_2015$coordflag) <- "Binary indicating missing coordinates"

table(mwi_cluster_2015$dhsregion, mwi_cluster_2015$coordflag)

# Check the lat/longs for any inconsistencies
mwi_cluster_2015 %>% 
  filter(coordflag != 1) %>% 
  plot(., max.plot = 1)

p2015 <- mwi_cluster_2015 %>%
  filter(coordflag != 1) %>%
  ggplot(.) +
  geom_sf(aes(color = dhsregion)) +
  scale_fill_viridis("Area") +
  ggtitle("DHS clusters by district 2015") +
  theme(legend.position = "none")


# Save the results to a new shapefile
setwd(here("GIS"))

st_write(mwi_cluster_2015, 
         "mwi_cluster_2015.shp", 
         delete_dsn = TRUE)


# 2010 Shapefile ----------------------------------------------------------
# Check out the 2010 data for spatial clusters and attributes
# Everything seems to be in order with the 2010 data, we'll just make district names proper
setwd(file.path(here(), dhs2010, "MWGE62FL"))
mwi_points_2010 <- st_read(dsn = '.', "MWGE62FL")

# Projection to use according to projectionwizard.org
# +proj=tcea +lon_0=34.27734375
#raster::crs(mwi_cluster_2010)

contents(mwi_points_2010)

# Clean up the region names to make them consistent across data sets
mwi_cluster_2010 <- mwi_points_2010 %>%
  mutate(dhsregion = str_to_title(DHSREGNA),
         dhsregion = ifelse(dhsregion == "Nkhota Kota", "Nkhotakota", dhsregion),
         dhsregion = ifelse(dhsregion == "Nkhatabay", "Nkhata Bay", dhsregion),
         coordflag = ifelse(LATNUM != 0, 0, 1))

label(mwi_cluster_2010$coordflag) <- "Binary indicating missing coordinates"
table(mwi_cluster_2010$dhsregion, mwi_cluster_2010$coordflag) 

# Check the lat/longs for any inconsistencies
# Notice that there are no points for the island ()
mwi_cluster_2010 %>% 
  filter(coordflag != 1) %>% 
  plot(., max.plot = 1)

p2010 <- mwi_cluster_2010 %>%
  filter(coordflag != 1) %>%
  ggplot(.) +
  geom_sf(aes(color = dhsregion)) +
  scale_fill_viridis("Area") +
  ggtitle("DHS clusters by district 2010") +
  theme(legend.position = "none") 

setwd(here("GIS"))

st_write(mwi_cluster_2010, 
         "mwi_cluster_2010.shp", 
         delete_dsn = TRUE)

quartz()
grid.arrange(p2015, p2010, nrow = 1)

# Check Districts for Consistency -----------------------------------------
# Check that the districts match up from each dataset
data_list = list(mwi_cluster_2010, mwi_cluster_2015)

dhs_check = map_df(data_list, 
       magrittr::extract, "dhsregion") 

table(dhs_check$dhsregion)

# drop the temp dataframe
remove(dhs_check)

# NOTES: Need to fix NkhotaKota -- line 47 above
# Nkhata Bay appears to be it's own district in the 


# Join kids data with point data to recovery district information ---------
# kids10$cluster variable == mwi_cluster_2010$DHSCLUST

# Extract out the key variables you need for spatial work and stats
tmp_2010 <- mwi_cluster_2010 %>% 
  select(LATNUM, LONGNUM, dhsregion, DHSCLUST, ALT_GPS, ALT_DEM, coordflag) %>% 
  as.data.frame(.) 
str(tmp_2010)

kids10_gis <- kids10 %>% 
  mutate(DHSCLUST = as.numeric(cluster)) %>% 
  left_join(., tmp_2010, by = c("DHSCLUST" = "DHSCLUST")) %>% 
  select(-geometry)

remove(tmp_2015)

# Now conduct similar process for 2015 data
tmp_2015 <- mwi_cluster_2015 %>% 
  select(LATNUM, LONGNUM, dhsregion, DHSCLUST, ALT_GPS, ALT_DEM, coordflag) %>% 
  as.data.frame(.) 
str(tmp_2015)

kids15_gis <- kids15 %>% 
  mutate(DHSCLUST = as.numeric(cluster)) %>% 
  left_join(., tmp_2015, by = c("DHSCLUST" = "DHSCLUST")) %>% 
  select(-geometry)



# Check the shapefiles out for merging w/ summary statistics in th --------

setwd(here())
setwd(here("GIS/mwi_2010_shp/shps/"))
mwi_shape_2010 <- st_read(dsn = '.', "sdr_subnational_boundaries2")
table(mwi_shape_2010$REGNAME)


# Fix the names to match the ones above
mwi_shape_2010 <- mwi_shape_2010 %>% 
  mutate(dhsregion = str_to_title(REGNAME),
         dhsregion = ifelse(dhsregion == "Nkhota Kota", "Nkhotakota", dhsregion),
         dhsregion = ifelse(dhsregion == "Nkhatabay", "Nkhata Bay", dhsregion))
table(mwi_shape_2010$dhsregion) 

# in theory, you should be able to join on the dhsregion variable
df1 = mwi_shape_2010 %>%
  select(dhsregion) %>% 
  group_by(dhsregion) %>% 
  count() %>% 
  as.data.frame() %>%  
  select(-geometry, df1 = dhsregion) 

df2 = kids10_gis %>% 
  select(dhsregion) %>% 
  group_by(dhsregion) %>% 
  count() %>% 
  left_join(., df1, by = c("dhsregion" = "df1"))

setdiff(kids10_gis$dhsregion, mwi_shape_2010$dhsregion)

# Now for the 2015 shapefile 
setwd(here())
setwd(here("GIS/mwi_2015_shp/shps"))
mwi_shape_2015 <- st_read(dsn = '.', "sdr_subnational_boundaries2")
table(mwi_shape_2015$REGNAME) 

# Magritter pipe operator makes this less repretitive
mwi_shape_2015 %<>% mutate(dhsregion = str_to_title(REGNAME),
                           dhsregion = ifelse(dhsregion == "Nkhota Kota", "Nkhotakota", dhsregion),
                           dhsregion = ifelse(dhsregion == "Mulange", "Mulanje", dhsregion),
                           dhsregion = ifelse(dhsregion == "Nkhatabay", "Nkhata Bay", dhsregion))


# Check for differences in districts -- seem to match just fine.
# So in theory, we should be able to make some maps now w/ the district info
setdiff(mwi_shape_2015$dhsregion, mwi_shape_2010$dhsregion)
setdiff(mwi_shape_2015$dhsregion, kids15_gis$dhsregion)

# plot the shapefile for a final test
colors_2010 <- length((unique(mwi_shape_2010$dhsregion)))
shp_2010 <- mwi_shape_2010 %>%
  mutate(
    lon = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~ st_centroid(.x)[[2]])
  ) %>%
  ggplot(.) +
  geom_sf(aes(fill = dhsregion), colour = "white") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(colors_2010)) +
  ggtitle("Malawi by district DHS 2010") +
  geom_text(aes(label = dhsregion, x = lon, y = lat),
            color = "#808080",
            size = 2) +
  theme(legend.position = "none")


colors_2015 <- length((unique(mwi_shape_2015$dhsregion)))
shp_2015 <- mwi_shape_2015 %>%
  mutate(
    lon = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
    lat = map_dbl(geometry, ~ st_centroid(.x)[[2]])
  ) %>%
  ggplot(.) +
  geom_sf(aes(fill = dhsregion), colour = "white") +
  scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(colors_2015)) +
  ggtitle("Malawi by district DHS 2015") +
  geom_text(aes(label = dhsregion, x = lon, y = lat),
            color = "#808080",
            size = 2) +
  theme(legend.position = "none")

# Combine points and polygons
#mapview::mapview(mwi_cluster_2015)

# Combine the two maps together to view the difference -- Likoma is the main difference in 2015
grid.arrange(shp_2010, shp_2015, nrow = 1)

# Save the new shapefiles for passing to WVU
setwd(here("GIS"))
st_write(mwi_shape_2010, 
         "MWI_DHS_2010.shp",
         delete_dsn = TRUE)

st_write(mwi_shape_2015, 
         "MWI_DHS_2015.shp",
         delete_dsn = TRUE)


# Create a few extracts for the WVU folks to do the interpolations
setwd(here())
kids10_gis %>%
  filter(elig == 1, coordflag != 1,!is.na(stunting) == 1) %>%
  select(
    LATNUM,
    LONGNUM,
    stunting,
    stunted,
    wasted,
    wasting,
    DHSCLUST,
    ALT_GPS,
    ALT_DEM,
    rural,
    svywt,
    year,
    dhsregion
  ) %>%
  write_csv(., path = here("Dataout/MWI_2010_Anthro.csv"))


kids15_gis %>%
  filter(elig == 1, coordflag != 1,!is.na(stunting) == 1) %>%
  select(
    LATNUM,
    LONGNUM,
    stunting,
    stunted,
    wasted,
    wasting,
    DHSCLUST,
    ALT_GPS,
    ALT_DEM,
    rural,
    svywt,
    year,
    dhsregion
  ) %>%
  write_csv(., path = here("Dataout/MWI_2015_Anthro.csv"))


# Write out the stunting data for exploration in Tableau
mwi_dhs_2010_anthro <- kids10_gis %>% 
  filter(!is.na(stunting)) %>% 
  select(stunting,
         rural, 
         interview_month, 
         age_mom,
         educ_high,
         cmc, 
         lit,
         hhsize, 
         kids_under5,
         femhead,
         agehead, 
         wealth, 
         wealth_cat, 
         wealth_cat_lab,
         mom_stunting,
         sex_lab,
         femhead_lab, 
         DHSCLUST,
         latitude = LATNUM,
         longitude = LONGNUM,
         dhsregion,
         ALT_GPS, ALT_DEM,
         coordflag,
         stunting, 
         stunted,
         wasting,
         wasted,
         bmi,
         year,
         birth_order, 
         birth_month)

mwi_dhs_2015_anthro <- kids15_gis %>% 
  filter(!is.na(stunting)) %>% 
  select(stunting,
         rural, 
         interview_month, 
         age_mom,
         educ_high,
         cmc, 
         lit,
         hhsize, 
         kids_under5,
         femhead,
         agehead, 
         wealth, 
         wealth_cat, 
         wealth_cat_lab,
         mom_stunting,
         sex_lab,
         femhead_lab, 
         DHSCLUST,
         latitude = LATNUM,
         longitude = LONGNUM,
         dhsregion,
         ALT_GPS, ALT_DEM,
         coordflag,
         stunting, 
         stunted,
         wasting,
         wasted,
         bmi,
         year,
         birth_order, 
         birth_month)


# Need to check if this crashes Tableau 
datalist =list(mwi_dhs_2010_anthro = mwi_dhs_2010_anthro, 
               mwi_dhs_2015_anthro = mwi_dhs_2015_anthro) 

# Paths likely need to be fixed on this to make it run smoothly
datalist %>%  
  names() %>% 
  map(., ~ write_csv(datalist[[.]], str_c(here(out_dir), ., ".csv")))


