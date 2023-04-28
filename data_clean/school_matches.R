library(sf)
library(raster)
library(readxl)
library(tidyverse)
library(stringr)
library(stringdist)
library(data.table)
setwd("C:/Users/garci/Dropbox/eab_chicago_data")

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
  st_transform(raster::crs(tree_loss))%>%
  dplyr::select(CountyName:NCES_ID)%>%
  rename(RCD = 3) %>%
  mutate(Dist_number = substrRight(RCD, 4),
         County = tolower(CountyName))%>%
  st_crop(extent(tree_loss))

county_list <- unique(ed_loc.sf$County)

school_loc <- ed_loc.sf %>%
  group_by(Dist_number)%>%
  filter(RecType != "Dist")%>%
  separate(GradeServe, into = c("grade_low", "grade_high"))%>%
  ungroup()%>%
  mutate(grade_low = ifelse(grade_low %in% c("K", "P"), 1, grade_low),
         grade_high = ifelse(grade_high %in% c("K", "P"), 1, grade_high),
         name.loc = word(tolower(FacilityNa), 1, 2),
         name.loc1 = word(tolower(FacilityNa), 1),
         name.loc2 = substr(word(tolower(FacilityNa), 2),1,2))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### read in test scores
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

files <- list.files(path = "schools/test_scores", recursive = TRUE,
                    pattern = ".xls",
                    full.names = TRUE
)

school_read_excel <- function(x){
  read_excel(x)%>%
    rename(Dist_number = `Dist #`)%>%
    group_by(Dist_number, County)
  #%>%
   # slice(2:n())
  
}

school_temp = sapply(files, school_read_excel, simplify = FALSE)



school_scores <- rbindlist(school_temp,
                           use.names = TRUE, idcol = "filename", 
                           fill = TRUE)%>%
  separate(filename, into = c(NA, NA, NA, NA, "Year"))%>%
  mutate(year = as.numeric(paste0("20", Year, sep = "")),
         County = tolower(County),
         name.score = word(tolower(`District Name/ School Name`), 1, 2),
         name.score1 = word(tolower(`District Name/ School Name`), 1),
         name.score2 = substr(word(tolower(`District Name/ School Name`), 2), 1, 2))%>%
  select(-Year)%>%
  filter(County %in% county_list)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### match the two
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match_distcounty <- school_loc %>%
  inner_join(school_scores, by = c("County", "Dist_number"))

filtered_matches <- match_distcounty %>%
  mutate(name_str = stringdist(name.loc, name.score),
         name_str1 = stringdist(name.loc1, name.score1),
         name_str2 = stringdist(name.loc2, name.score2))%>%
  group_by(name.loc)%>%
  filter(name_str1 == min(name_str1) & name_str2 == min(name_str2) & name_str1 <= 1 )%>%
  ungroup() %>%
  group_by(name.score)%>%
  filter(name_str == min(name_str) & name_str1 <= 1 )%>%
  select(name.loc, name.score, NCES_ID, Dist_number, Read_3:year, grade_low, grade_high)%>%
  mutate(my_id = cur_group_id())

library(rio)
export(filtered_matches, "schools/school_test_scores.rds")
