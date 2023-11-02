library(sf)
library(raster)
library(terra)
library(ncdf4)
library(readxl)
library(tidyverse)
library(exactextractr)
library(ggplot2)
library(kableExtra)
library(modelsummary)

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
#### Define roi - either Chicagoland or metro area
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
eab_infestations <- read_sf(paste0(data_dir,"eeb_infestations/eeb_address_geocode.shp"))%>%
  dplyr::select(Date, City, Match_addr, City_1, Address)%>%
  separate(Date, c("Year", NA, NA))%>%
  mutate(Year = as.numeric(Year))%>%
  st_transform(my_crs)

# place locations
places.shp <- st_read(paste0(data_dir,"administrative/tl_2017_17_place/tl_2017_17_place.shp"), quiet = TRUE)%>%
  st_transform(my_crs)%>%
  st_intersection(extent_roi)%>%
  st_sf()%>%
  st_join(eab_infestations)%>%
  group_by(NAME, PLACEFP)%>%
  mutate(place_first_detected = min(Year, na.rm = T))%>%
  dplyr::select(-c(Year))%>%
  ungroup 

# ACS data
ACS <- read.csv(paste0(data_dir,"ACS/ACS_censustract_5yr_0509.csv"))%>%
  mutate(GEOID10 = as.character(FIPS))

ACS.shp <- read_sf(paste0(data_dir,"administrative/tl_2010_17_tract/tl_2010_17_tract10.shp"))%>%
  st_transform(my_crs)%>%
  select(GEOID10, NAME10, NAMELSAD10)%>%
  left_join(ACS, by = "GEOID10")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
######## Maps showing study region and infestation occurrence
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(tmap)

roi_map <- tm_shape(illinois.shp) +
  tm_polygons(col = "lightgoldenrodyellow")+
  tm_shape(extent_roi) +
  tm_polygons(col = "grey85")


map_infestations <- eab_infestations %>%
  st_intersection(extent_roi)

Breaks = seq(from = min(eab_infestations$Year), to = 2014)
Labels = as.character(Breaks)

metro_eab <- tm_shape(extent_roi) +
  tm_polygons(col = "grey90") +
  tm_shape(map_infestations)+
  tm_symbols(col = "Year",
             breaks = Breaks, labels = Labels,
             palette = "plasma",
             legend.hist = T,
             size = 0.35)+
  tm_layout(legend.outside = TRUE) +
  tm_compass(type = "4star", size = 1, position = c("right", "top"))+
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20))
metro_eab

tmap_save(metro_eab, paste0(fig_dir, "/metro_infestations_map.png"), height = 5, width = 7)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Raster data on tree loss/gain and canopy cover
# SILVIS data

tree_gain <- raster::raster(paste0(data_dir, "/tree_data/tree_gain_year.tif"))%>%
  raster::crop(illinois.shp)
tree_loss <- raster::raster(paste0(data_dir, "/tree_data/tree_loss_year.tif"))%>%
  raster::crop(illinois.shp)
impervious_increase <- raster::raster(paste0(data_dir, "/tree_data/IS_change_year.tif"))%>%
  raster::crop(illinois.shp)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in Emapr data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Emapr data
canopy_filelist <- list.files(paste0(data_dir, 'tree_data/emapr/canopy_cover'), pattern = '.tif', full.names = TRUE)

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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Create grid
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
grid_sizes_km <- c(
  seq(from = 1, to = 10, by = 1)
  )

for(g in grid_sizes_km){
  
  grid_spacing = g*1000
  
  
  grid <- st_make_grid(extent_roi, square = T, cellsize = c(grid_spacing, grid_spacing), crs = my_crs) %>% # the grid, covering bounding box
    st_sf() %>% #not really required, but makes the grid nicer to work with later
    st_intersection(extent_roi)%>%
    mutate(grid = row_number())
  
  grid_place <- grid %>%
    st_intersection(places.shp %>% select(NAME, place_first_detected))%>%
    mutate(intersect_area = st_area(.),
           place_first_detected = ifelse(is.na(place_first_detected), 0, place_first_detected))%>%
    group_by(grid)%>%
    filter(intersect_area == max(intersect_area))%>%
    slice_head()%>%
    ungroup %>%
    mutate_at(vars(place_first_detected), ~replace(., is.na(.), 0))%>%
    mutate_at(vars(place_first_detected), ~replace(., is.infinite(.), 0))%>%
    select(grid, place_first_detected)
  grid_place$geometry <- NULL
  
  grid_centroids <- st_centroid(grid)%>%
    st_join(counties.shp)%>%
    left_join(ash_by_county, by = c("NAME" = "County"))%>%
    st_join(ACS.shp)%>%
    left_join(grid_place, by = "grid") %>%
    mutate_at(vars(place_first_detected), ~replace(., is.na(.), 0))
  grid_centroids$geometry <- NULL
  
  gridded_infestations <- grid %>%
    st_join(eab_infestations)%>%
    group_by(grid)%>%
    mutate(first_detected = min(Year, na.rm = T))%>%
    slice_head()%>%
    dplyr::select(grid, first_detected) %>%
    ungroup %>%
    mutate_at(vars(first_detected), ~replace(., is.na(.), 0))%>%
    mutate_at(vars(first_detected), ~replace(., is.infinite(.), 0))
  
  eab_loss_data <- gridded_infestations %>%
    left_join(grid_centroids, by = "grid")%>%
    mutate(place_first_detected = ifelse(place_first_detected == 0 , first_detected, place_first_detected))%>%
    select(grid, first_detected, place_first_detected, everything())
  
  Breaks = seq(from = 2006, to = 2014)
  Labels = c("No infestation", as.character(Breaks))
  
  tm_shape(gridded_infestations) +
    tm_polygons("first_detected",
                breaks = c(0, Breaks), labels = Labels,
                palette = "plasma")
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ##### extract canopy cover data
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  library(exactextractr)
  min_year = 2000
  max_year = 2015
  year_list <- seq(from = min_year, to = max_year, by = 1)
  
  for(i in year_list){
    
    this_tree_gain <- tree_gain
    this_tree_gain[this_tree_gain[] != i ] = 0
    this_tree_gain[this_tree_gain[] == i ] = 1
    
    new <- exact_extract(this_tree_gain, eab_loss_data, 'sum')
    eab_loss_data[ , ncol(eab_loss_data) + 1] <- new      # Append new column
    colnames(eab_loss_data)[ncol(eab_loss_data)] <- paste0("gain_", i)
    
    this_tree_loss <- tree_loss
    this_tree_loss[this_tree_loss[] != i ] = 0
    this_tree_loss[this_tree_loss[] == i ] = 1
    
    new <- exact_extract(this_tree_loss, eab_loss_data, 'sum')
    eab_loss_data[ , ncol(eab_loss_data) + 1] <- new      # Append new column
    colnames(eab_loss_data)[ncol(eab_loss_data)] <- paste0("loss_", i)
    
  }
  
  eab_data <- eab_loss_data
  
  
  min_canopy_year = 1990 + min(bands) - 1
  for(i in 1:length(bands)){
    
    # get year 
    year <- i + min_canopy_year - 1
    print(year)
    # extract from canopy cover raster
    this_canopy_cover <- canopy_raster_list[[i]]
    
    new <- terra::extract(this_canopy_cover, eab_data, 'mean') %>% select(-ID)
    eab_data[ , ncol(eab_data) + 1] <- new      # Append new column
    
    colnames(eab_data)[ncol(eab_data)] <- paste0("canopy_", year)
    
  }
  
  max_canopy_year = min_canopy_year + length(bands) - 1
  

  extracted_data <- eab_data
  
  extracted_data$geometry = NULL
  
  eab_panel <- extracted_data %>%
    mutate(canopy_baseline = canopy_2002)%>%
    pivot_longer(cols = paste0("gain_",min_year):paste0("canopy_",max_canopy_year),
                 names_to = "type_year", 
                 values_to = "total_change")%>%
    separate(type_year, into = c("change_type", "year"), sep = "_")%>%
    mutate_at(vars(year, first_detected, grid), as.numeric)%>%
    pivot_wider(names_from = "change_type", values_from = "total_change")%>%
    group_by(year, grid)%>%
    slice_head()%>%
    mutate(#net_gain = gain - loss,
      treated = ifelse(year >= first_detected & year > 0, 1, 0),
      e_time = ifelse(first_exposed > 0, year - first_exposed, 0)
      )
  
  library(rio)
  export(eab_panel, paste0(clean_data_dir, "/eab_panel_grid", g, "km.rds"))
  
}

