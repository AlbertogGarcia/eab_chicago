library(sf)
library(raster)
library(terra)
library(readxl)
library(tidyverse)
library(exactextractr)
library(ggplot2)

select <- dplyr::select

clean_data_dir <- here::here("cleaned")

fig_dir <- here::here("figs")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Administrative data

# illinois shapefile
illinois.shp <- read_sf(paste0(data_dir, "administrative/IL_State/IL_BNDY_State_Py.shp"))%>%
  st_transform(raster::crs(raster(paste0(data_dir, "tree_data/tree_loss_year.tif"))))

my_crs <- st_crs(illinois.shp)

#Chicago shapefile
chicago.shp <- read_sf(paste0(data_dir, "administrative/chicago_citylimits/geo_export_3a192531-441a-46d7-8c19-37beb7617696.shp"))%>%
  st_transform(my_crs)%>%
  mutate(NAME = "Chicago",
         STATEFP = 17)%>%
  select(NAME, STATEFP)

# Chicago area counties separating out chicago limits
counties.shp <- read_sf(paste0(data_dir, "administrative/tl_2019_us_county/tl_2019_us_county.shp"))%>%
  st_transform(my_crs)%>%
  st_difference(chicago.shp)%>%
  select(NAME, STATEFP)%>%
  rbind(chicago.shp)%>%
  st_intersection(illinois.shp)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Define roi - either 7 county region
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

counties_chicago7 <- c("Cook", "DuPage", "Kane", "Kendall", "Lake", "McHenry", "Will")
roi_counties <- counties_chicago7

chicago.shp <- read_sf(paste0(data_dir, "administrative/chicago_citylimits/geo_export_3a192531-441a-46d7-8c19-37beb7617696.shp"))%>%
  st_transform(st_crs(illinois.shp))%>%
  mutate(NAME = "Chicago",
         STATEFP = 17)%>%
  select(NAME, STATEFP)

counties.shp <- read_sf(paste0(data_dir, "administrative/tl_2019_us_county/tl_2019_us_county.shp"))%>%
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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in Ash borer and other data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ash density by county in chicago region
ash_by_county <- read.csv(paste0(data_dir, "tree_data/ash_by_county.csv"))

# Geocoded confirmed EAB infestations
eab_infestations <- read_sf(paste0(data_dir, "eeb_infestations/eeb_address_geocode.shp"))%>%
  dplyr::select(Date, City)%>%
  rename(city_infestation = City)%>%
  separate(Date, c("Year", NA, NA))%>%
  mutate(Year = as.numeric(Year))%>%
  st_transform(my_crs)

# place locations
places.shp <- st_read(paste0(data_dir, "administrative/tl_2017_17_place/tl_2017_17_place.shp"), quiet = TRUE)%>%
  st_transform(my_crs)%>%
  st_intersection(extent_roi)%>%
  st_sf()%>%
  st_join(eab_infestations)%>%
  group_by(NAME, PLACEFP)%>%
  mutate(place_first_detected = min(Year, na.rm = T))%>%
  dplyr::select(-c(Year))%>%
  ungroup 

# ACS data
ACS <- read.csv(paste0(data_dir, "ACS/ACS_censustract_5yr_0509.csv"))%>%
  mutate(GEOID10 = as.character(FIPS))

ACS.shp <- read_sf(paste0(data_dir, "administrative/tl_2010_17_tract/tl_2010_17_tract10.shp"))%>%
  st_transform(my_crs)%>%
  select(GEOID10, NAME10, NAMELSAD10)%>%
  left_join(ACS, by = "GEOID10")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Raster data on tree loss/gain and canopy cover
# SILVIS data

tree_gain <- raster::raster(paste0(data_dir, "tree_data/tree_gain_year.tif"))%>%
  raster::crop(illinois.shp)
tree_loss <- raster::raster(paste0(data_dir, "tree_data/tree_loss_year.tif"))%>%
  raster::crop(illinois.shp)
impervious_increase <- raster::raster(paste0(data_dir, "tree_data/IS_change_year.tif"))%>%
  raster::crop(illinois.shp)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in Emapr data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Emapr data
canopy_filelist <- list.files(paste0(data_dir, 'tree_data/emapr/canopy_cover'), pattern = '.tif', full.names = TRUE)

biomass_filelist <- list.files(paste0(data_dir, 'tree_data/emapr/biomass'), pattern = '.tif', full.names = TRUE)
min_bands <- 2000 - 1990 + 1
max_bands <- 2015 - 1990 + 1
bands <- min_bands:max_bands

read_emapr <- function(bands, file_list){
  
  for(b in bands){
    
    for(i in 1:length(file_list)){
      # get file name 
      file_name <- file_list[i]
      
      # read in raster
      rast_i <- terra::rast(file_name, lyrs = b)
      
      if(i == 1){
        
        this_raster <- rast_i
        
      } else {
        
        # merge rasters
        this_raster <- terra::merge(this_raster, rast_i)
        
      }
    }
    
    this_raster <- terra::project(this_raster, terra::rast(paste0(data_dir, "tree_data/tree_loss_year.tif")))
    
    if(b == min(bands)){
      
      raster_list <- this_raster
      
    } else {
      
      # concatonate rasters
      raster_list <- c(raster_list, this_raster)
      
    }
    
  }
  
  return(raster_list)
}

canopy_raster_list <- read_emapr(bands, canopy_filelist)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in School locations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(nngeo)
publicschool_loc <- read_sf(paste0(clean_data_dir, "/publicschool_loc.shp"))%>%
  st_transform(my_crs)%>%
  select(RCDS, RCD)%>%
  st_crop(extent_roi)

### Create buffer around schools from which to determine infestation exposure

meters_per_mile = 1609.34
treatment_buffer = meters_per_mile * 2

school_infestation <- publicschool_loc %>%
  st_join(counties.shp %>% select(NAME))%>%
  left_join(ash_by_county, by = c("NAME" = "County"))%>%
  st_join(ACS.shp)%>%
  mutate(dist_to_infestation_site = unlist(st_nn(., eab_infestations, k = 1, returnDist = T)[[2]]))%>%
  st_join(st_buffer(eab_infestations, treatment_buffer))%>%
  group_by(RCDS)%>%
  mutate(first_exposed = min(as.numeric(Year), na.rm = T))%>%
  mutate_at(vars(first_exposed), ~replace(., is.na(.) | is.infinite(.), 0))%>%
  slice_head()%>%
  ungroup %>%
  select(- Year)

school_infestation$geometry <- NULL

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Add in report card data (education outcome data)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

control_vars <- c("school - white pct", "school - black pct", "school - hispanic pct", "school - asian pct", "low-income school pct", "l.e.p. school pct"
                  , "all_attendance rate school pct",
                  "chronic truants rate school pct",
                  "school total enrollment"
)

reportcard_data <- readRDS(paste0(clean_data_dir, "/reportcard_data.rds"))%>%
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
##### extract canopy cover data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(exactextractr)
min_year = 1995
max_year = 2015
year_list <- seq(from = min_year, to = max_year, by = 1)

publicschool_buffer <- publicschool_loc %>%
  st_crop(extent_roi) %>%
  st_buffer(treatment_buffer)

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
  print(year)
  
  colnames(publicschool_buffer)[ncol(publicschool_buffer)] <- paste0("canopy_", year)
  
}

extracted_data <- publicschool_buffer 
  
extracted_data$geometry = NULL


max_canopy_year = min_canopy_year + length(bands) - 1


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
  left_join(canopy_panel, by = c("RCDS", "RCD"))%>%
  #left_join(reportcard_data, by = c("RCDS", "year"))%>%
  inner_join(reportcard_data, by = c("RCDS", "year"))%>%
  group_by(RCDS, year)%>%
  slice_head()%>%
  ungroup %>%
  group_by(RCDS)%>%
  mutate(school_ID = cur_group_id())%>%
  ungroup

eab_panel_school <- eab_panel %>%
  rename(white_pct = "school - white pct",
         black_pct = "school - black pct",
         hispanic_pct = "school - hispanic pct",
         asian_pct = "school - asian pct",
         lowinc_pct = "low-income school pct",
         cov_white_pct = "cov_school - white pct",
         cov_black_pct = "cov_school - black pct",
         cov_hispanic_pct = "cov_school - hispanic pct",
         cov_asian_pct = "cov_school - asian pct",
         cov_lowinc_pct = "cov_low-income school pct",
         cov_lep_pct = "cov_l.e.p. school pct",
         cov_truants_pct = "cov_chronic truants rate school pct",
         cov_all_attend = "cov_all_attendance rate school pct",
         cov_enrollment = "cov_school total enrollment"
  )%>%
  mutate(low_income_attend = `low income_attendance rate school pct`,
         all_attend = `all_attendance rate school pct`,
         enrollment = `school total enrollment`,
         net_gain = gain - loss,
         treated = ifelse(first_exposed > 0 & year >= first_exposed, 1, 0))%>%
  mutate_at(vars(school_ID, year, first_exposed,
                 all_tests,
                 ISAT_composite,
                 low_income_attend,
                 all_attend,
                 white_pct, black_pct, hispanic_pct, asian_pct, lowinc_pct, enrollment,
                 year, first_exposed
  ),
  as.numeric)%>%
  mutate(e_time = ifelse(first_exposed > 0, year - first_exposed, 0))


library(rio)

export(eab_panel_school, paste0(clean_data_dir, "/eab_panel_school2km.rds"))


