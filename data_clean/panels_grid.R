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
library(beepr)
library(gstat)
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

ash_density_predicted <- terra::rast(paste0(clean_data_dir, "/ash_density.tif"))

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
library(basemaps)

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





get_maptypes()

extent_roi_webmerc <- extent_roi %>%
  st_transform(crs = 3857)

map_infestations_webmerc<- map_infestations %>%
  st_transform(crs = 3857)

bg <- basemaps::basemap_terra(ext=extent_roi_webmerc, 
                               map_service = "carto", map_type = "light_no_labels"
                               )


bg_labels <- basemaps::basemap_terra(ext=extent_roi_webmerc, 
                               map_service = "carto", map_type = "light_only_labels"
)

metro_street_eab <- tm_shape(bg) + tm_rgb() +
  tm_shape(extent_roi_webmerc) +
  tm_polygons(alpha = 0, border.col = "black") +
  tm_shape(bg_labels) + tm_rgb() +
  tm_shape(map_infestations_webmerc)+
  tm_symbols(col = "Year",
             breaks = Breaks, labels = Labels,
             palette = "plasma",
             legend.hist = F,
             size = 0.35)+
  tm_layout(legend.outside = TRUE) +
  tm_compass(type = "4star", size = 1, position = c("right", "top"))+
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20))
metro_street_eab

tmap_save(metro_street_eab, paste0(fig_dir, "/metro1_infestations_map.png"), height = 5, width = 7)


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
#### Read in Emapr data fcn
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



read_emapr <- function(bands, file_list){
  
  for(b in bands){
    print(1990 + b)
    
    r_list <- list()
    for(i in 1:length(file_list)){
      
      # get file name
      file_name <- file_list[i]
      print(i)

      # read in raster
      rast_i <- terra::rast(file_name, lyrs = b)

      r_list[[length(r_list)+1]] = rast_i

    }
    
    merged_rast <- do.call(terra::merge, r_list) %>%
      terra::project(extent_roi)
    
    if(b == min(bands)){
      
      raster_list <- merged_rast
      
    } else {
      
      # concatonate rasters
      raster_list <- c(raster_list, merged_rast)
      
    }
    
  }
  
  return(raster_list)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Canopy cover
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#Emapr data
canopy_filelist <- list.files(paste0(data_dir, 'tree_data/emapr/canopy_cover'), pattern = '.tif', full.names = TRUE)

min_canopy_year = 2000
min_bands <- min_canopy_year - 1990 + 1
max_bands <- 2015 - 1990 + 1
bands <- min_bands:max_bands

canopy_raster_list <- read_emapr(bands, canopy_filelist)

canopy_raster_2006 <- canopy_raster_list[[2006 - min_canopy_year]] %>%
  terra::mask(roi)

metro_canopy_eab <- tm_shape(extent_roi) +
  tm_fill("white")+
  tm_shape(raster(canopy_raster_2006))+
  tm_raster(pal = c("white", "#006D2C"),
            title = "Canopy cover probability (2006)")+
  tm_shape(extent_roi) +
  tm_polygons(alpha = 0, border.col = "black") +
  tm_shape(map_infestations)+
  tm_symbols(col = "Year",
             breaks = Breaks, labels = Labels,
             palette = "plasma",
             legend.hist = T,
             size = 0.35)+
  tm_layout(legend.outside = TRUE) +
  tm_compass(type = "4star", size = 1, position = c("right", "top"))+
  tm_scale_bar(breaks = c(0, 5, 10, 15, 20))
metro_canopy_eab
tmap_save(metro_canopy_eab, paste0(fig_dir, "/metro_canopy_infestations_map.png"), height = 5, width = 7)

beep(1)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Create grid
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
grid_sizes_km <- c(
  0.25, 0.5, 0.75,
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
  
  # add in predicted ash by grid
  mean_ash <- terra::extract(ash_density_predicted, eab_loss_data, 'mean') %>% select(-ID) %>% rename(ash_density_predicted = 1)
  eab_loss_data[ , ncol(eab_loss_data) + 1] <- mean_ash  
  
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
    mutate(canopy_05 = canopy_2005,
           canopy_0600 = canopy_2006 - canopy_2000,
           canopy_0602 = canopy_2006 - canopy_2002,
           canopy_0604 = canopy_2006 - canopy_2004
    )%>%
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
      e_time = ifelse(first_detected > 0, year - first_detected, 0)
      )
  
  library(rio)
  export(eab_panel, paste0(clean_data_dir, "/eab_panel_grid", g, "km.rds"))
  
}

beep(1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### INTERPOLATED GRID
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
map <- extent_roi %>% st_sf()
grid_detect <- terra::rast(map, nrows = 1000, ncols = 1000)
xy <- terra::xyFromCell(grid_detect, 1:ncell(grid_detect))

coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"),
                 crs = st_crs(map))
coop <- st_filter(coop, map)

#qtm(coop)

# Nearest neighbors
res <- gstat(formula = Year ~ 1, locations = eab_infestations, nmax = 5,
             set = list(idp = 0))

resp <- predict(res, coop)
resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
resp$pred <- resp$var1.pred

pred <- terra::rasterize(resp, grid_detect, field = "pred", fun = "mean")

interpolated_map <- tm_shape(pred) + 
  tm_raster(alpha = 0.65, palette = "viridis",
            title = ' ', 
            labels = as.character(seq(from = 2006, to = 2015, by = 1)),
            breaks = seq(from = 2006, to = 2015, by = 1)
            )+
  tm_layout(legend.position = c("right", "top"), 
                       #title= 'Arrival of ash borer', 
                       #title.position = c('right', 'top')
            )
interpolated_map

tmap_save(interpolated_map, paste0(fig_dir, "/interpolated_infestations_map.png"), height = 6, width = 9)


g = 1
grid_spacing = g*1000

grid <- st_make_grid(extent_roi, square = T, cellsize = c(grid_spacing, grid_spacing), crs = my_crs) %>% # the grid, covering bounding box
  st_sf() %>% #not really required, but makes the grid nicer to work with later
  st_intersection(extent_roi)%>%
  mutate(grid = row_number())

grid_centroids <- st_centroid(grid)%>%
  st_join(counties.shp)%>%
  left_join(ash_by_county, by = c("NAME" = "County"))%>%
  st_join(ACS.shp)
grid_centroids$geometry <- NULL

mean <- terra::extract(pred, grid, 'mean') %>% select(-ID) %>% rename(first_detected_mean = 1)
grid[ , ncol(grid) + 1] <- mean  
min <- terra::extract(pred, grid, 'min') %>% select(-ID)%>% rename(first_detected_min = 1)
grid[ , ncol(grid) + 1] <- min   

grid <- grid %>%
  mutate(first_detected = floor(first_detected_mean),
         first_detected_min = floor(first_detected_min))
  
eab_loss_data <- grid %>%
  left_join(grid_centroids, by = "grid")%>%
  select(grid, everything())


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
  mutate(canopy_05 = canopy_2005,
         canopy_0600 = canopy_2006 - canopy_2000,
         canopy_0602 = canopy_2006 - canopy_2002,
         canopy_0604 = canopy_2006 - canopy_2004
         )%>%
  pivot_longer(cols = paste0("canopy_",min_canopy_year):paste0("canopy_",max_canopy_year),
               names_to = "type_year", 
               values_to = "total_change")%>%
  separate(type_year, into = c("change_type", "year"), sep = "_")%>%
  mutate_at(vars(year, first_detected, first_detected_min, grid), as.numeric)%>%
  pivot_wider(names_from = "change_type", values_from = "total_change")%>%
  group_by(year, grid)%>%
  slice_head()%>%
  mutate(treated = ifelse(year >= first_detected & year > 0, 1, 0),
    e_time = ifelse(first_detected > 0, year - first_detected, 0)
  )

export(eab_panel, paste0(clean_data_dir, "/eab_panel_grid_interpolated_", g, "km.rds"))


beep(2)