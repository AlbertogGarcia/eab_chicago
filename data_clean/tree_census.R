library(sf)
library(raster)
library(terra)
library(tidyverse)
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


#### Tree census
chicago_trees <- read_sf(paste0(data_dir, "MortonArboretum/June2019/June2019.shp"))%>%
  st_drop_geometry()%>%
  filter(YEAR <= 2013)%>%
  mutate(ash = ifelse(GENUS == "Fraxinus", 1, 0))%>%
  drop_na(GENUS)%>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
           crs = 4326)%>%
  st_transform(st_crs(extent_roi))

ash_mean <- mean(chicago_trees$ash)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### INTERPOLATED GRID
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(gstat)
library(tmap)
map <- extent_roi %>% st_sf()
grid <- terra::rast(map, nrows = 500, ncols = 500)
xy <- terra::xyFromCell(grid, 1:ncell(grid))

coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"),
                 crs = st_crs(map))
coop <- st_filter(coop, map)

#qtm(coop)

# Inverse distance weighting
ivd <- gstat(formula = ash ~ 1, locations = chicago_trees,
             nmax = nrow(chicago_trees), # use all the neighbors locations
             set = list(idp = 1)) # beta = 1
# p1 <- predict(ivd, coop)$var1.pred
# # Nearest neighbors
nn <- gstat(formula = ash ~ 1, locations = chicago_trees, nmax = 5,
             set = list(idp = 0))
# p2 <- predict(nn, coop)$var1.pred
# 
# weights <- c(1/2, 1/2)
# p3 <- p1 * weights[1] + p2 * weights[2]
# 
# resp <- data.frame(
#   x = st_coordinates(coop)[, 1],
#   y = st_coordinates(coop)[, 2],
#   pred = p3)
# 
# resp <- st_as_sf(resp, coords = c("x", "y"), crs = st_crs(map))
# 
# pred_combo <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
# tm_shape(pred_combo) + tm_raster(alpha = 0.6, palette = "viridis")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
# alternatively just pick one
res = nn

resp <- predict(res, coop)
resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
resp$pred <- resp$var1.pred

pred <- terra::rasterize(resp, grid, field = "pred", fun = "mean")
tm_shape(pred) + 
  tm_raster(alpha = 0.6, palette = "viridis")
  #+ tm_shape(chicago_trees) + tm_dots(size = 0.1)
  #+ 

writeRaster(pred,paste0(clean_data_dir, "/ash_density.tif"),gdal=c("COMPRESS=NONE", "TFW=YES"), overwrite = T)
