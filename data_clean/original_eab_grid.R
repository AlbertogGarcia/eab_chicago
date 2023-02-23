library(sf)
library(raster)
library(terra)
library(readxl)
library(tidyverse)
library(exactextractr)
library(ggplot2)

setwd("C:/Users/agarcia/Dropbox/chicago_eab")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Read in data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Administrative data

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


### Raster data on tree loss/gain and canopy cover
# SILVIS data
tree_loss <- raster("tree_data/tree_loss_year.tif")%>%
  raster::crop(illinois.shp)
tree_gain <- raster("tree_data/tree_gain_year.tif")%>%
  raster::crop(illinois.shp)
impervious_increase <- raster("tree_data/IS_change_year.tif")%>%
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
  st_transform(my_crs)

# ACS data
ACS <- read.csv("ACS/ACS_censustract_5yr_0509.csv")%>%
  mutate(GEOID10 = as.character(FIPS))

ACS.shp <- read_sf("administrative/tl_2010_17_tract/tl_2010_17_tract10.shp")%>%
  st_transform(st_crs(eab_infestations))%>%
  select(GEOID10, NAME10, NAMELSAD10)%>%
  left_join(ACS, by = "GEOID10")

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
  
  canopy_raster <- canopy_raster %>%
    terra::project(tree_loss)#%>%
  # terra::crop(tree_loss)
  
  if(b == min(bands)){
    
    canopy_raster_list <- canopy_raster
    
  } else {
    
    # merge rasters
    canopy_raster_list <- c(canopy_raster_list, canopy_raster)
    
  }
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Create grid
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
grid_spacing = 3000
#grid_spacing = 5000

silvis_bound <- st_bbox(tree_loss)

grid <- st_make_grid(silvis_bound, square = T, cellsize = c(grid_spacing, grid_spacing), crs = st_crs(eab_infestations)) %>% # the grid, covering bounding box
  st_sf() %>%#not really required, but makes the grid nicer to work with later
  mutate(grid = row_number())

grid_centroids <- st_centroid(grid)%>%
  st_join(counties.shp)%>%
  left_join(ash_by_county, by = c("NAME" = "County"))%>%
  st_join(ACS.shp)
grid_centroids$geometry <- NULL

gridded_infestations <- grid %>%
  st_join(eab_infestations)%>%
  group_by(grid)%>%
  mutate(first_detected = min(Year, na.rm = T))%>%
  dplyr::select(-c(Year))
#st_write(gridded_infestations, "gridded_infestations.shp")

ggplot(eab_infestations %>% st_crop(gridded_infestations), aes(color = Year))+
  geom_sf(size = 1.5)
ggplot(gridded_infestations , aes(fill = first_detected))+
  geom_sf(size = 1.5)



eeb_loss_data <- gridded_infestations %>%
  st_crop(tree_loss)%>%
  mutate_at(vars(first_detected), ~replace(., is.na(.), 0))%>%
  left_join(grid_centroids, by = "grid")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### extract canopy cover data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(exactextractr)
min_year = 1995
max_year = 2015
year_list <- seq(from = min_year, to = max_year, by = 1)

for(i in year_list){
  
  this_tree_gain <- tree_gain
  this_tree_gain[this_tree_gain[] != i ] = 0
  this_tree_gain[this_tree_gain[] == i ] = 1
  
  new <- exact_extract(this_tree_gain, eeb_loss_data, 'sum')
  eeb_loss_data[ , ncol(eeb_loss_data) + 1] <- new      # Append new column
  colnames(eeb_loss_data)[ncol(eeb_loss_data)] <- paste0("gain_", i)
  
  this_tree_loss <- tree_loss
  this_tree_loss[this_tree_loss[] != i ] = 0
  this_tree_loss[this_tree_loss[] == i ] = 1
  
  new <- exact_extract(this_tree_loss, eeb_loss_data, 'sum')
  eeb_loss_data[ , ncol(eeb_loss_data) + 1] <- new      # Append new column
  colnames(eeb_loss_data)[ncol(eeb_loss_data)] <- paste0("loss_", i)
  
  this_impervious_increase <- impervious_increase
  this_impervious_increase[this_impervious_increase[] != i ] = 0
  this_impervious_increase[this_impervious_increase[] == i ] = 1
  
  new <- exact_extract(this_impervious_increase, eeb_loss_data, 'sum')
  eeb_loss_data[ , ncol(eeb_loss_data) + 1] <- new      # Append new column
  colnames(eeb_loss_data)[ncol(eeb_loss_data)] <- paste0("impervious_", i)
  
}

eab_data <- eeb_loss_data

min_canopy_year = 1990 + min(bands) - 1

for(i in 1:length(bands)){
  
  this_canopy_cover <- canopy_raster_list[[i]]
  
  new <- exact_extract(this_canopy_cover, eab_data, 'mean')
  eab_data[ , ncol(eab_data) + 1] <- new      # Append new column
  
  year <- i + min_canopy_year - 1
  
  colnames(eab_data)[ncol(eab_data)] <- paste0("canopy_", year)
  
}

max_canopy_year = min_canopy_year + length(bands) - 1

extracted_data <- eab_data

extracted_data$geometry = NULL

eab_panel <- extracted_data %>%
  pivot_longer(cols = paste0("gain_",min_year):paste0("impervious_",max_year),
               names_to = "type_year", 
               values_to = "total_change")%>%
  separate(type_year, into = c("change_type", "year"), sep = "_")%>%
  mutate_at(vars(year, first_detected, grid), as.numeric)%>%
  pivot_wider(names_from = "change_type", values_from = "total_change")%>%
  group_by(year, grid)%>%
  slice_head()%>%
  mutate(net_gain = gain - loss,
         treated = ifelse(year > first_detected & year > 0, 1, 0))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### DID estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(did)
set.seed(0930)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### tree loss

loss_attgt <- att_gt(yname = "loss",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = eab_panel
)
summary(loss_attgt)
loss_ovr <- aggte(loss_attgt, type = "simple")
summary(loss_ovr)
loss_es <- aggte(loss_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "loss", "ATT" = loss_ovr$overall.att, "se" = loss_ovr$overall.se)
es_results <- data.frame("outcome" = "loss", "ATT" = loss_es$att.egt, "e" = loss_es$egt, "se" = loss_es$se.egt)

library(fixest)
twfe_loss <- feols(loss ~ treated | year + grid, data = eab_panel)
summary(twfe_loss)

# loss by ash density in county
twfe_loss <- feols(loss ~ treated * pct_trees_in_area | year + grid, data = eab_panel)
summary(twfe_loss)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###  Tree cover change

net_attgt <- att_gt(yname = "net_gain",
                    tname = "year",
                    idname = "grid",
                    gname = "first_detected",
                    control_group = "notyettreated",
                    data = eab_panel
)


net_ovr <- aggte(net_attgt, type = "simple")
summary(net_ovr)
net_es <- aggte(net_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "net gain", "ATT" = net_ovr$overall.att, "se" = net_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "net gain", "ATT" = net_es$att.egt, "e" = net_es$egt, "se" = net_es$se.egt)%>%
  rbind(es_results)

twfe_net <- feols(net_gain ~ treated | year + grid, data = eab_panel)
summary(twfe_net)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Tree cover gain

gain_attgt <- att_gt(yname = "gain",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = eeb_panel
)
summary(gain_attgt)

gain_ovr <- aggte(gain_attgt, type = "simple")
summary(gain_ovr)
gain_es <- aggte(gain_attgt, type = "dynamic")
twfe_cum <- feols(gain ~ treated | year + grid, data = eeb_panel)
summary(twfe_cum)

ovr_results <- data.frame("outcome" = "gain", "ATT" = gain_ovr$overall.att, "se" = gain_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "gain", "ATT" = gain_es$att.egt, "e" = gain_es$egt, "se" = gain_es$se.egt)%>%
  rbind(es_results)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Event study plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loss_plot <- ggplot(es_results %>% filter(outcome == "loss" & e >= -10), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=lowerci,ymax=upperci),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
loss_plot

netcover_plot <- ggplot(es_results %>% filter(outcome == "net gain" & e >= -10), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=lowerci,ymax=upperci),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
netcover_plot

gain_plot <- ggplot(es_results %>% filter(outcome == "gain" & e >= -10), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=lowerci,ymax=upperci),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
gain_plot