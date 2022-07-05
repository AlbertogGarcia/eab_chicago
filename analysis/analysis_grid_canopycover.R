library(sf)
library(raster)
library(readxl)
library(tidyverse)
library(here)
library(did)

select <- dplyr::select
source(here::here('analysis', 'schart.R'))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Read in spatial data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setwd("C:/Users/garci/Dropbox/eab_chicago_data")

counties_chicagoreg7 <- c("Cook", "DuPage", "Kane", "Kendall", "Lake", "McHenry", "Will")

raster_filelist <- list.files('tree_data/canopy_cover', pattern = '.tif', full.names = TRUE)
bands <- 28

illinois.shp <- read_sf("administrative/IL_State/IL_BNDY_State_Py.shp")%>%
  st_transform(crs(raster(raster_filelist[1])))

roi <- read_sf("administrative/tl_2019_us_county/tl_2019_us_county.shp")%>%
  st_transform(crs(raster(raster_filelist[1])))%>%
  filter(STATEFP == 17 & NAME %in% counties_chicagoreg7)%>%
  st_intersection(illinois.shp)

extent_roi <- illinois.shp %>%
  st_intersection(roi)

for(b in 1:bands){
  
  for(i in 1:length(raster_filelist)){
    # get file name 
    file_name <- raster_filelist[i]
    
    # read in raster
    rast_i <- raster(file_name, band = b)
    
    if(i == 1){
      
      canopy_raster <- rast_i
      
    } else {
      
      # merge rasters
      canopy_raster <- merge(canopy_raster, rast_i)
      
      
    }
  }
  
  canopy_raster <- crop(canopy_raster, roi)
  
  # assign(paste0("canopy_raster_", b), canopy_raster)
  
  if(b == 1){
    
    canopy_raster_list <- canopy_raster
    
  } else {
    
    # merge rasters
    canopy_raster_list <- c(canopy_raster_list, canopy_raster)
    
    
  }
  
}



eab_infestations <- read_sf("eeb_infestations/eeb_address_geocode.shp")%>%
  dplyr::select(Date, City, Match_addr, City_1, Address)%>%
  separate(Date, c("Year", "Month", NA))%>%
  st_transform(crs(roi))%>%
  st_intersection(roi)

plot <- ggplot()+
  geom_sf(data = roi, fill = "papayawhip")+
  geom_sf(data = eab_infestations, aes(color = Year),size = 3)+
  labs(title = "  Confirmed EAB infestation locations within region of interest",
    fill = "Infestation year") + 
  theme_void()
plot

# grid_spacing = 3000
grid_spacing = 5000


gridded_infestations <- st_make_grid(roi, square = T, cellsize = c(grid_spacing, grid_spacing), crs = st_crs(eab_infestations)) %>% # the grid, covering bounding box
  st_sf() %>%# not really required, but makes the grid nicer to work with later
  mutate(grid = row_number())%>%
  st_join(eab_infestations)%>%
  group_by(grid)%>%
  mutate(first_detected = min(Year, na.rm = T))%>%
  slice_head()%>%
  dplyr::select(-c(Year))%>%
  ungroup()%>%
  st_intersection(extent_roi)

plot <- ggplot()+
  geom_sf(data = gridded_infestations, aes(fill = first_detected))+
  labs(title = "  Year of first EAB detection by grid cell",
       fill = "First detection year") + 
  theme_void()
plot

#st_write(gridded_infestations, "gridded_infestations.shp")

# ggplot(eab_infestations %>% st_crop(gridded_infestations), aes(color = Year))+
#   geom_sf(size = 1.5)
# ggplot(gridded_infestations , aes(fill = first_detected))+
#   geom_sf(size = 1.5)



eab_canopy_data <- gridded_infestations %>%
  mutate_at(vars(first_detected), ~replace(., is.na(.), 0))

library(exactextractr)
min_year = 1990

for(i in 1:bands){
  
  this_canopy_cover <- canopy_raster_list[[i]]
  
  new <- exact_extract(this_canopy_cover, eab_canopy_data, 'mean')
  eab_canopy_data[ , ncol(eab_canopy_data) + 1] <- new      # Append new column
  
  year <- i + min_year - 1
  
  colnames(eab_canopy_data)[ncol(eab_canopy_data)] <- paste0("canopy_", year)
  
}

max_year = min_year + max(bands) - 1

eab_canopy_data$geometry = NULL

eab_panel <- eab_canopy_data %>%
  pivot_longer(cols = paste0("canopy_",min_year):paste0("canopy_",max_year),
               names_to = "type_year", 
               values_to = "canopy_cover_pct")%>%
  separate(type_year, into = c(NA, "year"), sep = "_")%>%
  mutate_at(vars(year, first_detected, grid), as.numeric)%>%
  group_by(year, grid)%>%
  slice_head()%>%
  mutate(treated = ifelse(year > first_detected & year > 0, 1, 0))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### canopy cover
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(did)
set.seed(0930)
canopy_attgt <- att_gt(yname = "canopy_cover_pct",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = eab_panel
)
canopy_ovr <- aggte(canopy_attgt, type = "simple")
summary(canopy_ovr)
canopy_es <- aggte(canopy_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "canopy_cover_pct", "ATT" = canopy_ovr$overall.att, "se" = canopy_ovr$overall.se)
es_results <- data.frame("outcome" = "canopy_cover_pct", "ATT" = canopy_es$att.egt, "e" = canopy_es$egt, "se" = canopy_es$se.egt, "simul_crit" = es$crit.val.egt)%>%
  mutate(lowerci = ATT - 1.96 * se,
         upperci = ATT + 1.96 * se)

library(fixest)
twfe <- feols(canopy_cover_pct ~ treated | year + grid, data = eeb_panel)
summary(twfe)





palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520")

canopy_plot <- ggplot(es_results %>% filter(outcome == "canopy_cover_pct" & e < 10), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=lowerci,ymax=upperci),alpha=0.2, fill = palette$red)+
  geom_vline(xintercept = -0.75, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+
  ggtitle("canopy cover (percent)")
canopy_plot

