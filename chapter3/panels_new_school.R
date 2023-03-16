library(sf)
library(raster)
library(terra)
library(readxl)
library(tidyverse)
library(exactextractr)
library(ggplot2)

select <- dplyr::select
setwd("C:/Users/garci/Dropbox/eab_chicago_data")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# illinois shapefile
illinois.shp <- read_sf("administrative/IL_State/IL_BNDY_State_Py.shp")%>%
  st_transform(raster::crs(raster("tree_data/tree_loss_year.tif")))

my_crs <- st_crs(illinois.shp)

#Chicago shapefile
chicago.shp <- read_sf("administrative/chicago_citylimits/geo_export_3a192531-441a-46d7-8c19-37beb7617696.shp")%>%
  st_transform(my_crs)%>%
  mutate(NAME = "Chicago",
         STATEFP = 17)%>%
  select(NAME, STATEFP)

# Chicago area counties separating out chicago limits
counties.shp <- read_sf("administrative/tl_2019_us_county/tl_2019_us_county.shp")%>%
  st_transform(my_crs)%>%
  st_difference(chicago.shp)%>%
  select(NAME, STATEFP)%>%
  rbind(chicago.shp)%>%
  st_intersection(illinois.shp)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Define roi - either Chicagoland or metro area
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

counties_chicago7 <- c("Cook", "DuPage", "Kane", "Kendall", "Lake", "McHenry", "Will")
roi_counties <- counties_chicago7

chicago.shp <- read_sf("administrative/chicago_citylimits/geo_export_3a192531-441a-46d7-8c19-37beb7617696.shp")%>%
  st_transform(st_crs(illinois.shp))%>%
  mutate(NAME = "Chicago",
         STATEFP = 17)%>%
  select(NAME, STATEFP)

counties.shp <- read_sf("administrative/tl_2019_us_county/tl_2019_us_county.shp")%>%
  st_transform(my_crs)%>%
  st_difference(chicago.shp)%>%
  select(NAME, STATEFP)%>%
  rbind(chicago.shp)%>%
  st_intersection(illinois.shp)

roi <- counties.shp %>%  
  filter(STATEFP == 17 & NAME %in% c(roi_counties, "Chicago"))%>%
  rename(County = NAME)

extent_roi <- illinois.shp %>%
  st_intersection(roi)%>%
  st_union()

### Raster data on tree loss/gain and canopy cover
# SILVIS data

tree_gain <- raster::raster("tree_data/tree_gain_year.tif")%>%
  raster::crop(illinois.shp)
tree_loss <- raster::raster("tree_data/tree_loss_year.tif")%>%
  raster::crop(illinois.shp)

#Emapr data
raster_filelist <- list.files('tree_data/canopy_cover', pattern = '.tif', full.names = TRUE)
min_bands <- 2000 - 1990 + 1
bands <- min_bands:28

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in Ash borer and other data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ash density by county in chicago region
ash_by_county <- read.csv("tree_data/ash_by_county.csv")

# Geocoded confirmed EAB infestations
eab_infestations <- read_sf("eeb_infestations/eeb_address_geocode.shp")%>%
  dplyr::select(Date, City, Match_addr, City_1, Address)%>%
  separate(Date, c("Year", NA, NA))%>%
  st_transform(my_crs)%>%
  select(Year, City)

# ACS data
ACS <- read.csv("ACS/ACS_censustract_5yr_0509.csv")%>%
  mutate(GEOID10 = as.character(FIPS))

ACS.shp <- read_sf("administrative/tl_2010_17_tract/tl_2010_17_tract10.shp")%>%
  st_transform(st_crs(eab_infestations))%>%
  select(GEOID10, NAME10, NAMELSAD10)%>%
  left_join(ACS, by = "GEOID10")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in School locations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(nngeo)
publicschool_loc <- read_sf("schools/cleaned/publicschool_loc.shp")%>%
  st_transform(my_crs)%>%
  select(RCDS)%>%
  st_crop(extent_roi)

### Create buffer around schools from which to determine infestation exposure
buffer_size = 3000

school_infestation <- publicschool_loc %>%
  st_join(counties.shp %>% select(NAME))%>%
  left_join(ash_by_county, by = c("NAME" = "County"))%>%
  st_join(ACS.shp)%>%
  mutate(dist_to_infestation_site = unlist(st_nn(., eab_infestations, k = 1, returnDist = T)[[2]]))%>%
  st_join(st_buffer(eab_infestations, buffer_size))%>%
  group_by(RCDS)%>%
  mutate(first_exposed = min(as.numeric(Year), na.rm = T))%>%
  mutate_at(vars(first_exposed), ~replace(., is.infinite(.), 0))

school_infestation$geometry <- NULL

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Add in report card data (education outcome data)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

control_vars <- c("school - white pct", "school - black pct", "school - hispanic pct", "school - asian pct", "low-income school pct", "l.e.p. school pct"
                  , "all_attendance rate school pct",
                  "chronic truants rate school pct",
                  "school total enrollment"
)

reportcard_data <- readRDS("schools/cleaned/reportcard_data.rds")%>%
  mutate(year = as.numeric(as.character(year)))

minyear = 2000
maxyear = 2005
for(i in 1:length(control_vars)){
  print(i)
  x <- control_vars[i]
  print(x)
  df_temp <- reportcard_data %>%
    filter(between(year, minyear, maxyear))%>%
    group_by(RCDS)%>%
    mutate_at(vars(x), ~ mean(as.numeric(.), na.rm = T))%>%
    slice_head()%>%
    select(RCDS, x) %>%
    slice_head()
  
  names(df_temp) <- c(names(df_temp)[1], paste0("cov_", x))
  
  reportcard_data <- left_join(reportcard_data, df_temp, by = "RCDS")
  
}

covs_school <- paste0("cov_", seq(from = 1, to = length(control_vars), by = 1))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read and clean canopy cover data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for(b in bands){
  
  for(i in 1:length(raster_filelist)){
    # get file name 
    file_name <- raster_filelist[i]
    
    # read in raster
    rast_i <- terra::rast(file_name, lyrs = b)
    
    if(i == 1){
      
      canopy_raster <- rast_i
      
    } else {
      
      # merge rasters
      canopy_raster <- terra::merge(canopy_raster, rast_i)
      
    }
  }
  
  canopy_raster <- terra::project(canopy_raster, terra::rast("tree_data/tree_loss_year.tif"))
  
  if(b == min(bands)){
    
    canopy_raster_list <- canopy_raster
    
  } else {
    
    # concatonate rasters
    canopy_raster_list <- c(canopy_raster_list, canopy_raster)
    
  }
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### extract canopy cover data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

publicschool_buffer <- st_buffer(publicschool_loc, buffer_size)%>%
  st_crop(extent_roi)

library(exactextractr)
min_year = 1995
max_year = 2015
year_list <- seq(from = min_year, to = max_year, by = 1)

for(i in year_list){
  
  this_tree_gain <- tree_gain
  this_tree_gain[this_tree_gain[] != i ] = 0
  this_tree_gain[this_tree_gain[] == i ] = 1
  
  new <- exact_extract(this_tree_gain, publicschool_buffer, 'sum')
  publicschool_buffer[ , ncol(publicschool_buffer) + 1] <- new      # Append new column
  colnames(publicschool_buffer)[ncol(publicschool_buffer)] <- paste0("gain_", i)
  
  this_tree_loss <- tree_loss
  this_tree_loss[this_tree_loss[] != i ] = 0
  this_tree_loss[this_tree_loss[] == i ] = 1
  
  new <- exact_extract(this_tree_loss, publicschool_buffer, 'sum')
  publicschool_buffer[ , ncol(publicschool_buffer) + 1] <- new      # Append new column
  colnames(publicschool_buffer)[ncol(publicschool_buffer)] <- paste0("loss_", i)
  
}


min_canopy_year = 1990 + min(bands) - 1

for(i in 1:length(bands)){
  
  this_canopy_cover <- canopy_raster_list[[i]]
  
  new <- terra::extract(this_canopy_cover, publicschool_buffer, 'mean') %>% select(-ID)
  publicschool_buffer[ , ncol(publicschool_buffer) + 1] <- new      # Append new column
  
  year <- i + min_canopy_year - 1
  
  colnames(publicschool_buffer)[ncol(publicschool_buffer)] <- paste0("canopy_", year)
  
}

max_canopy_year = min_canopy_year + length(bands) - 1

extracted_school_data <- publicschool_buffer

extracted_school_data$geometry = NULL

library(rio)
export(extracted_school_data, "schools/cleaned/extracted_school_data.rds")


canopy_panel <- extracted_school_data %>%
  mutate(canopy_baseline = canopy_2006)%>%
  pivot_longer(cols = paste0("gain_",min_year):paste0("canopy_",max_canopy_year),
               names_to = "type_year", 
               values_to = "total_change")%>%
  separate(type_year, into = c("change_type", "year"), sep = "_")%>%
  pivot_wider(names_from = "change_type", values_from = "total_change")%>%
  mutate(year = as.numeric(year))%>%
  filter(between(year, 2003, 2014))

eab_panel <- school_infestation %>%
  left_join(canopy_panel, by = c("RCDS"))%>%
  left_join(reportcard_data, by = c("RCDS", "year"))%>%
  group_by(RCDS)%>%  
  mutate(ID = as.numeric(cur_group_id()),
         year = as.character(year))%>%
  select(ID, year, everything())%>%
  ungroup %>%
  group_by(year, ID)%>%
  slice_head()

library(rio)
export(eab_panel, "output/eab_panel_school3km.rds")


