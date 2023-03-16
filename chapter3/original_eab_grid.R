library(sf)
library(raster)
library(terra)
library(readxl)
library(tidyverse)
library(exactextractr)
library(ggplot2)
library(kableExtra)
library(modelsummary)

setwd("C:/Users/garci/Dropbox/eab_chicago_data")

results_dir <- "output"

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
impervious_increase <- raster::raster("tree_data/IS_change_year.tif")%>%
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
  
  canopy_raster <- terra::project(canopy_raster, terra::rast("tree_data/tree_loss_year.tif"))
  
  if(b == min(bands)){
    
    canopy_raster_list <- canopy_raster
    
  } else {
    
    # concatonate rasters
    canopy_raster_list <- c(canopy_raster_list, canopy_raster)
    
  }
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Create grid
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
grid_spacing = 2500
#grid_spacing = 5000

silvis_bound <- st_bbox(tree_loss)


grid <- st_make_grid(extent_roi, square = T, cellsize = c(grid_spacing, grid_spacing), crs = st_crs(eab_infestations)) %>% # the grid, covering bounding box
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
  mutate_at(vars(first_detected), ~replace(., is.na(.), 0))%>%
  left_join(grid_centroids, by = "grid")%>%
  ungroup

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
  
  new <- terra::extract(this_canopy_cover, eab_data, 'mean') %>% select(-ID)
  eab_data[ , ncol(eab_data) + 1] <- new      # Append new column
  
  year <- i + min_canopy_year - 1
  
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
  mutate(net_gain = gain - loss,
         treated = ifelse(year > first_detected & year > 0, 1, 0))

library(rio)
export(eab_panel, "output/eab_panel_grid25km.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### DID estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(did)
set.seed(0930)

eab_panel <- readRDS("output/eab_panel_grid3km.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### tree loss

loss_attgt <- att_gt(yname = "loss",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = eab_panel
)
loss_ovr <- aggte(loss_attgt, type = "simple")
summary(loss_ovr)
loss_es <- aggte(loss_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "loss", "ATT" = loss_ovr$overall.att, "se" = loss_ovr$overall.se)
es_results <- data.frame("outcome" = "loss", "ATT" = loss_es$att.egt, "e" = loss_es$egt, "se" = loss_es$se.egt)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Tree cover gain

gain_attgt <- att_gt(yname = "gain",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = eab_panel
)
gain_ovr <- aggte(gain_attgt, type = "simple")
summary(gain_ovr)
gain_es <- aggte(gain_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "gain", "ATT" = gain_ovr$overall.att, "se" = gain_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "gain", "ATT" = gain_es$att.egt, "e" = gain_es$egt, "se" = gain_es$se.egt)%>%
  rbind(es_results)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Canopy cover

canopy_attgt <- att_gt(yname = "canopy",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     xformla = ~ pct_trees_in_area + med_household_income + poverty_pct ,
                     ,
                     data = eab_panel
)
canopy_ovr <- aggte(canopy_attgt, type = "simple")
summary(canopy_ovr)
canopy_es <- aggte(canopy_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "canopy", "ATT" = canopy_ovr$overall.att, "se" = canopy_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "canopy", "ATT" = canopy_es$att.egt, "e" = canopy_es$egt, "se" = canopy_es$se.egt)%>%
  rbind(es_results)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Event study plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loss_plot <- ggplot(es_results %>% filter(outcome == "loss" & between(e, -7, 8)), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=ATT - 1.96*se,ymax=ATT + 1.96*se),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
loss_plot

gain_plot <- ggplot(es_results %>% filter(outcome == "gain" & between(e, -7, 8)), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=ATT - 1.96*se,ymax=ATT + 1.96*se),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
gain_plot


canopy_plot <- ggplot(es_results %>% filter(outcome == "canopy" & between(e, -7, 8)), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=ATT - 1.96*se,ymax=ATT + 1.96*se),alpha=0.2)+
  #geom_ribbon(aes(ymin=ATT - canopy_es$crit.val.egt*se,ymax=ATT + canopy_es$crit.val.egt*se),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
canopy_plot

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### TWFE results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(fixest)

# Main results

twfe_canopy <- feols(canopy ~ treated | year + grid, data = eab_panel)
summary(twfe_canopy)

twfe_loss <- feols(loss ~ treated | year + grid, data = eab_panel)
summary(twfe_loss)

twfe_gain <- feols(gain ~ treated | year + grid, data = eab_panel)
summary(twfe_gain)

models = list("Loss" = twfe_loss,
              "Gain" = twfe_gain,
              "Canopy cover" = twfe_canopy
)

treat_canopy_2005 <- round(mean(subset(eab_panel, year == 2005 & first_detected > 0)$canopy, na.rm = T)  , digits = 3)
treat_loss <- round(mean(subset(eab_panel, year < first_detected & first_detected > 0)$loss, na.rm = T)  , digits = 3)
treat_gain <- round(mean(subset(eab_panel, year < first_detected & first_detected > 0)$gain, na.rm = T)  , digits = 3)


rows <- tribble(~term, ~`Canopy cover`, ~Loss, ~Gain, 
                'pre-treatment mean', as.character(treat_canopy_2005), as.character(treat_loss), as.character(treat_gain)
)%>%
  as.data.frame()

f1 <- function(x) format(round(x, 4), big.mark=",")
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models,
             output="latex",
             title = 'TWFE estimates of canopy cover impacts of ash borer infestation',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~grid,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = c("treated" = "Infestation"),
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the grid level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "twfe_main_3km.tex"))


# Heterogeneity in canopy cover impacts

twfe_canopy_ash <- feols(canopy ~ treated * log(trees_per_acre) | year + grid, data = eab_panel)
summary(twfe_canopy_ash)

twfe_canopy_baseline <- feols(canopy ~ treated * log(canopy_baseline) | year + grid, data = eab_panel)
summary(twfe_canopy_baseline)

twfe_canopy_income <- feols(canopy ~ treated + treated:log(med_household_income) | year + grid, data = eab_panel)
summary(twfe_canopy_income)

twfe_canopy_it <- feols(canopy ~ treated + treated:log(med_household_income) + treated:log(trees_per_acre) +  treated:log(canopy_baseline) | year + grid, data = eab_panel)
summary(twfe_canopy_it)

models = list("(1)" = twfe_canopy_ash,
              "(2)" = twfe_canopy_baseline,
              "(3)" = twfe_canopy_income,
              "(4)"  = twfe_canopy_it
)

modelsummary(models,
             output="latex",
             title = 'Heterogeneous canopy cover impacts of ash borer infestation',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~grid,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = c("treated" = "Infestation"),
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
         #    , add_rows = rows
             , notes = "Standard errors are clustered at the grid level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "twfe_het_3km.tex"))