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
bands <- 1:28

illinois.shp <- read_sf("administrative/IL_State/IL_BNDY_State_Py.shp")%>%
  st_transform(crs(raster(raster_filelist[1])))

chicago.shp <- read_sf("administrative/chicago_citylimits/geo_export_3a192531-441a-46d7-8c19-37beb7617696.shp")%>%
  st_transform(st_crs(illinois.shp))

roi <- read_sf("administrative/tl_2019_us_county/tl_2019_us_county.shp")%>%
  st_transform(st_crs(illinois.shp))%>%
  filter(STATEFP == 17 & NAME %in% counties_chicagoreg7)%>%
  st_intersection(illinois.shp)%>%
  st_difference(chicago.shp)

sd.shp <- st_read("schools/SCHOOLDISTRICT_SY1314_TL15/schooldistrict_sy1314_tl15.shp", type = 6)%>%
  filter(STATEFP == 17)%>%
  st_transform(st_crs(roi))

extent_roi <- illinois.shp %>%
  st_intersection(roi)


thing_to_loop = bands
pb <- progress_bar$new(format = " [:bar] :percent eta: :eta",total=length(thing_to_loop),clear=FALSE,width=60)

for(b in bands){
  pb$tick()
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
  
  if(b == min(bands)){
    
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
  st_intersection(extent_roi)%>%
  rename(cNAME = NAME)


reportcard_loc <- read_sf("schools/reportcard/cleaned/reportcard_loc.shp")%>%
  st_transform(crs(roi))%>%
  st_intersection(extent_roi)%>%
  select(RCDS, dstrctn)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Extract tree loss and gain within school district
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sds_within_roi <- (sd.shp %>% st_intersection(roi))$NAME

all_sd <- sd.shp %>%
  mutate(sd_type = ifelse(is.na(UNSDLEA), ifelse(is.na(SCSDLEA), "ELEMENTARY", "HIGH SCHOOL"), "UNIT"),
         sd_area = st_area(sd.shp))%>%
  select(NAME, GEOID, ELSDLEA, SCSDLEA, UNSDLEA, sd_type, sd_area)%>%
  st_cast("POLYGON")%>%
  filter(NAME %in% sds_within_roi)

library(exactextractr)
min_year = 1990 + min(bands) - 1

for(i in 1:length(bands)){
  
  
  year <- i + min_year - 1
  
  this_canopy_cover <- canopy_raster_list[[i]]
  
  new <- exact_extract(this_canopy_cover, all_sd, 'mean')
  all_sd[ , ncol(all_sd) + 1] <- new      # Append new column
  colnames(all_sd)[ncol(all_sd)] <- paste0("canopy_", year)

}

max_year = min_year + max(bands) - 1

sd_canopy <- all_sd %>%
  mutate(poly_area = st_area(all_sd),
         poly_proportion = poly_area / sd_area)%>%
  mutate_at(vars(paste0("canopy_",min_year):paste0("canopy_",max_year)), ~ poly_proportion * .)%>%
  group_by(sd_type, NAME, GEOID)%>%
  summarise_at(vars(paste0("canopy_",min_year):paste0("canopy_",max_year)), 
                sum
               )
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Determine district treatment year
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sd_exposure <- sd_canopy %>%
  st_join(eab_infestations)%>%
  group_by(NAME, GEOID)%>%
  mutate_at(vars(Year), as.numeric)%>%
  mutate(first_detected = min(Year, na.rm = T),
         exposed = ifelse(as.numeric(Month) > 5, Year + 1, Year),
         first_exposed = min(exposed, na.rm = T))%>%
  slice_head()%>%
  mutate_at(vars(first_exposed, first_detected), ~replace(., is.na(.), 0))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### sd panel
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sd_panel <- sd_exposure %>%
  mutate(canopy_baseline = canopy_2005,
         median_baseline = median(canopy_baseline),
         low_baseline = ifelse(canopy_baseline < median_baseline, 1, 0),
         high_baseline = ifelse(canopy_baseline > median_baseline, 1, 0))%>%
  pivot_longer(cols = paste0("canopy_",min_year):paste0("canopy_",max_year),
               names_to = "type_year", 
               values_to = "canopy_cover_pct")%>%
  separate(type_year, into = c(NA, "year"), sep = "_")%>%
  mutate_at(vars(year, first_detected, first_exposed), as.numeric)%>%
  group_by(year, NAME, GEOID)%>%
  slice_head()%>%
  mutate(treated_tree = ifelse(year >= first_detected & first_detected > 0, 1, 0),
         treated_test = ifelse(year >= first_exposed & first_exposed > 0, 1, 0))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Match each school to its district boundaries
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


unified_sd <- all_sd %>%
  filter(sd_type == "UNIT") %>%
  st_intersection(roi)

elem_sd <- all_sd %>%
  filter(sd_type == "ELEMENTARY") %>%
  st_intersection(roi)

sec_sd <- all_sd %>%
  filter(sd_type == "HIGH SCHOOL") %>%
  st_intersection(roi)




unified <- unifiedSD.shp %>%
  st_join(reportcard_loc %>% filter(dstrctn == "UNIT"))

elem <- elemSD.shp %>%
  st_join(reportcard_loc %>% filter(dstrctn == "ELEMENTARY"))

sec <- secSD.shp %>%
  st_join(reportcard_loc %>% filter(dstrctn == "HIGH SCHOOL"))
  
schools_districts <- rbind(unified, elem, sec)


 
school_exposure$geometry <- NULL
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### create panel
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eab_panel <- school_exposure %>%
  mutate(canopy_baseline = canopy_2005,
         median_baseline = median(canopy_baseline),
         low_baseline = ifelse(canopy_baseline < median_baseline, 1, 0),
         high_baseline = ifelse(canopy_baseline > median_baseline, 1, 0))%>%
  pivot_longer(cols = paste0("canopy_",min_year):paste0("canopy_",max_year),
               names_to = "type_year", 
               values_to = "canopy_cover_pct")%>%
  separate(type_year, into = c(NA, "year"), sep = "_")%>%
  mutate_at(vars(year, first_detected, first_exposed), as.numeric)%>%
  group_by(year, RCDS)%>%
  slice_head()%>%
  mutate(treated_tree = ifelse(year >= first_detected & first_detected > 0, 1, 0),
         treated_test = ifelse(year >= first_exposed & first_exposed > 0, 1, 0))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Add in report card data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

control_vars <- c("school - white pct", "school - black pct", "school - hispanic pct", "low-income school pct", "l.e.p. school pct")

reportcard_data <- readRDS("schools/reportcard/cleaned/reportcard_data.rds")

maxyear = 2005
for(i in 1:length(control_vars)){
  print(i)
  x <- control_vars[i]
  print(x)
  df_temp <- reportcard_data %>%
    filter(year <= maxyear)%>%
    group_by(RCDS)%>%
    mutate_at(vars(x), ~ mean(as.numeric(.), na.rm = T))%>%
    select(RCDS, x) %>%
    slice_head()
  
  names(df_temp) <- c(names(df_temp)[1], paste0("cov_", i))
  
  reportcard_data <- inner_join(reportcard_data, df_temp, by = "RCDS")
  
}



panel_full <- eab_panel %>%
  left_join(reportcard_data, by = c("RCDS", "year"))%>%
  group_by(RCDS, year)%>%    
  slice_head()




covs_school <- paste0("cov_", seq(from = 1, to = length(control_vars), by = 1))
cov_names <- c(covs_school, "canopy_baseline", "high_baseline")

# test <- c("ISAT_")
# groups <- c("all_")#, "low income_", "non-low income_")#, "white_", "black_", "hispanic_")
# grades <- c("gr3", "gr4", "gr5", "gr6", "gr7", "gr8")
# categ <- c(" read school below", " math school below", " read school academic warning", " math school academic warning")
# tests <- as.vector(outer(as.vector(outer(test, groups, paste0)), as.vector(outer(grades, categ, paste0)), paste0))



# outcome_names <- c(var_names, tests)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Descriptive Statistics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### DID Tree cover analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


panel <- panel_full %>%
  group_by(RCDS)%>%
  mutate(ID = cur_group_id())%>%    
  mutate_at(vars(ID, canopy_cover_pct), as.numeric)

attgt <- att_gt(yname = "canopy_cover_pct",
                tname = "year",
                idname = "ID",
                gname = "first_detected",
                control_group = "notyettreated",
                xformla = as.formula(paste("~ ", paste(cov_names, collapse = " + "))),
                data = panel
)

ovr <- aggte(attgt, type = "simple", na.rm = T)

ovr_results <- data.frame("outcome" = "canopy_cover_pct", "ATT" = ovr$overall.att, "se" = ovr$overall.se)

es <- aggte(attgt, type = "dynamic", na.rm = T)

es_results <- data.frame("outcome" = "canopy_cover_pct", "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "simul_crit" = es$crit.val.egt)


ovr_results <- ovr_results %>%
  mutate(crit.val = ATT/se)

tree_plot <- ggplot(es_results %>% filter(between(e, -15, 10))
                    ,
                    aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - simul_crit*se, ymax=ATT + simul_crit*se), alpha=0.2)+
  #geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.3, color = "red")+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
tree_plot


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### DID Report card analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reportcard_outcomevars <- c("all_attendance rate school pct",    
                            "white_attendance rate school pct",  "black_attendance rate school pct", 
                            "low income_attendance rate school pct",         
                            "dropout rate  school pct", "all_tests",                  
                            "ISAT_composite" ) 

ovr_results2 <- data.frame()
es_results2 <- data.frame()


for(k in reportcard_outcomevars){
  print(k)
  panel <- panel %>%
    mutate_at(vars(k), as.numeric)
  
  attgt <- att_gt(yname = k,
                  tname = "year",
                  idname = "ID",
                  gname = "first_exposed",
                  control_group = "notyettreated",
                  xformla = as.formula(paste("~ ", paste(covs_school, collapse = " + "))),
                  data = panel
  )
  
  ovr <- aggte(attgt, type = "simple", na.rm = T)
  
  ovr_results2 <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se)%>%
    rbind(ovr_results2)
  
  es <- aggte(attgt, type = "dynamic", na.rm = T)
  
  es_results2 <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "simul_crit" = es$crit.val.egt)%>%
    rbind(es_results2)
  
}

ovr_results <- ovr_results %>%
  mutate(crit.val = ATT/se)


all_tests_plot <- ggplot(es_results2 %>% filter(outcome == "all_tests",
                                                between(e, -8, 7)),
                         aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
all_tests_plot

ISAT_plot <- ggplot(es_results2 %>% filter(outcome == "ISAT_composite",
                                           between(e, -8, 7)),
                    aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
ISAT_plot


library(fixest)
twfe <- feols(all_tests ~ treated_test | year + ID, data = panel)
summary(twfe)

twfe <- feols(all_tests ~ treated_test * canopy_cover_pct | year + ID, data = panel)
summary(twfe)

twfe <- feols(all_tests ~ treated_test * as.numeric(`low-income school pct`) | year + ID, data = panel)
summary(twfe)


