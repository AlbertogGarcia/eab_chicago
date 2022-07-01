library(sf)
library(raster)
library(readxl)
library(tidyverse)
setwd("C:/Users/agarcia/Dropbox/eab_chicago_data")

illinois.shp <- read_sf("administrative/IL_State/IL_BNDY_State_Ln.shp")%>%
  st_transform(crs(raster("tree_data/tree_loss_year.tif")))
tree_loss <- raster("tree_data/tree_loss_year.tif")%>%
  crop(illinois.shp)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in school coordinates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

ed_loc.sf <- read_sf("schools/geocoded_schools/school_locations.shp")%>%
  st_transform(crs(tree_loss))%>%
  select(CountyName:NCES_ID)%>%
  rename(RCD = 3) %>%
  mutate(Dist_number = substrRight(RCD, 4),
         County = tolower(CountyName))

school_districts.shp <- read_sf("schools/SCHOOLDISTRICT_SY1314_TL15/schooldistrict_sy1314_tl15.shp")%>%
  st_transform(crs(tree_loss))%>%
  filter(STATEFP == 17)%>%
  st_crop(extent(tree_loss))%>%
  st_join(ed_loc.sf)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in test scores
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

files <- list.files(path = "schools/test_scores", recursive = TRUE,
                    pattern = ".xls",
                    full.names = TRUE
)

sdist_read_excel <- function(x){
  read_excel(x)%>%
    rename(Dist_number = `Dist #`)%>%
    group_by(Dist_number, County)%>%
    slice_head()
  
}

school_read_excel <- function(x){
  read_excel(x)%>%
    rename(Dist_number = `Dist #`)%>%
    group_by(Dist_number, County)%>%
    slice(2:n())
  
}

sdtemp = sapply(files, sdist_read_excel, simplify = FALSE)
school_temp = sapply(files, school_read_excel, simplify = FALSE)

library(data.table)

district_scores <- rbindlist(sdtemp,
                         use.names = TRUE, idcol = "filename", 
                         fill = TRUE)%>%
  separate(filename, into = c(NA, NA, NA, NA, "Year"))%>%
  mutate(year = as.numeric(paste0("20", Year, sep = "")),
         County = tolower(County))%>%
  select(-Year)

library(rio)

school_scores <- rbindlist(school_temp,
                             use.names = TRUE, idcol = "filename", 
                             fill = TRUE)%>%
  separate(filename, into = c(NA, NA, NA, NA, "Year"))%>%
  mutate(year = as.numeric(paste0("20", Year, sep = "")),
         County = tolower(County))%>%
  select(-Year)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### match the two
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
district_matches <- school_districts.shp %>%
  inner_join(district_scores, by = c("Dist_number", "County"))%>%
  select(GEOID, Read_3:year)

district_matches$geometry <- NULL
library(rio)
export(district_matches, "district_matches.rds")


