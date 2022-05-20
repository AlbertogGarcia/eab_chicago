library(sf)
library(raster)
library(readxl)
library(tidyverse)
library(here)

setwd("C:/Users/garci/Dropbox/eab_chicago_data")

illinois.shp <- read_sf("administrative/IL_State/IL_BNDY_State_Py.shp")%>%
  st_transform(crs(raster("tree_data/tree_loss_year.tif")))
tree_loss <- raster("tree_data/tree_loss_year.tif")%>%
  crop(illinois.shp)
tree_gain <- raster("tree_data/tree_gain_year.tif")%>%
  crop(illinois.shp)
impervious_increase <- raster("tree_data/IS_change_year.tif")%>%
  crop(illinois.shp)

silvis_bound <- st_bbox(tree_loss)
box <- st_make_grid(silvis_bound, n = c(1,1))
illinois_box <- illinois.shp %>%
  st_crop(box)

school_test_scores <- readRDS("schools/school_test_scores.rds")

buffer_size <- 3000

school_buffer <- st_crop(st_buffer(
  school_test_scores %>%
    group_by(my_id)%>%
    slice_head()%>%
    select(my_id)
    , buffer_size),
  tree_loss)

library(exactextractr)
min_year = 1995
max_year = 2015
year_list <- seq(from = min_year, to = max_year, by = 1)

for(i in year_list){
  print(i)
  this_tree_gain <- tree_gain
  this_tree_gain[this_tree_gain[] != i ] = 0
  this_tree_gain[this_tree_gain[] == i ] = 1
  
  new <- exact_extract(this_tree_gain, school_buffer, 'sum')
  school_buffer[ , ncol(school_buffer) + 1] <- new      # Append new column
  colnames(school_buffer)[ncol(school_buffer)] <- paste0("gain_", i)
  
  this_tree_loss <- tree_loss
  this_tree_loss[this_tree_loss[] != i ] = 0
  this_tree_loss[this_tree_loss[] == i ] = 1
  
  new <- exact_extract(this_tree_loss, school_buffer, 'sum')
  school_buffer[ , ncol(school_buffer) + 1] <- new      # Append new column
  colnames(school_buffer)[ncol(school_buffer)] <- paste0("loss_", i)
  
  # this_impervious_increase <- impervious_increase
  # this_impervious_increase[this_impervious_increase[] != i ] = 0
  # this_impervious_increase[this_impervious_increase[] == i ] = 1
  # 
  # new <- exact_extract(this_impervious_increase, school_buffer, 'sum')
  # school_buffer[ , ncol(school_buffer) + 1] <- new      # Append new column
  # colnames(school_buffer)[ncol(school_buffer)] <- paste0("impervious_", i)
  
}


eab_infestations <- read_sf("eeb_infestations/eeb_address_geocode.shp")%>%
  dplyr::select(Date, City, Match_addr, City_1, Address)%>%
  separate(Date, c("Year", NA, NA))%>%
  st_transform(crs(tree_loss))

school_infestation <- school_buffer %>%
  st_join(eab_infestations)%>%
  group_by(my_id)%>%
  mutate_at(vars(Year, my_id), as.numeric)%>%
  mutate(first_detected = min(Year, na.rm = T))%>%
  mutate_at(vars(first_detected), ~replace(., is.na(.) | is.infinite(.), 0))%>%
  mutate(char_first_detected = ifelse(first_detected == 0, "never", as.character(first_detected)))%>%
  filter(first_detected == min(first_detected))%>%
  slice_head()%>%
  ungroup()%>%
  select(-Year)

plot <- ggplot()+
  geom_sf(data = illinois_box, fill = "papayawhip")+
  geom_sf(data = school_infestation , color = "grey80", size = .75, fill = NA)+
  geom_sf(data = school_infestation %>% st_centroid(), aes(color = char_first_detected), size = 2.5, fill = NA)+
  labs(title = "Year of school's exposure to infestation (within 5km)",
       color = "School exposure year") + 
  theme_void()
plot


school_infestation$geometry <- NULL

schooltree_panel <- school_infestation %>%
  pivot_longer(cols = paste0("gain_",min_year):paste0("loss_",max_year),
               names_to = "type_year", 
               values_to = "total_change")%>%
  separate(type_year, into = c("change_type", "year"), sep = "_")%>%
  mutate_at(vars(year, first_detected, my_id), as.numeric)%>%
  pivot_wider(names_from = "change_type", values_from = "total_change")%>%
  group_by(year, my_id)%>%
  slice_head()%>%
  mutate(net_gain = gain - loss,
         acres_gain = gain * 0.222395,
         acres_loss = loss * 0.222395,
         acres_net_gain = net_gain * 0.222395
         )

school_test_scores$geometry <- NULL

full_panel <- schooltree_panel %>%
  left_join(school_test_scores, by = c("my_id", "year"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####### School tree cover impacts
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
scoretree_panel <- full_panel %>%
  group_by(my_id)%>%
  arrange(year, .by_group = T)%>%
  mutate(cumloss = cumsum(acres_loss),
         cumgain = cumsum(acres_gain),
         cumnet = cumsum(acres_net_gain)
  )


library(did)
set.seed(0930)
loss_attgt <- att_gt(yname = "acres_loss",
                     tname = "year",
                     idname = "my_id",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = scoretree_panel
)
loss_ovr <- aggte(loss_attgt, type = "simple")
summary(loss_ovr)
loss_es <- aggte(loss_attgt, type = "dynamic")

schooltree_ovr_results <- data.frame("outcome" = "loss", "ATT" = loss_ovr$overall.att, "se" = loss_ovr$overall.se)

schooltree_es_results <- data.frame("outcome" = "loss", "ATT" = loss_es$att.egt, "e" = loss_es$egt, "se" = loss_es$se.egt)


gain_attgt <- att_gt(yname = "acres_gain",
                     tname = "year",
                     idname = "my_id",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = scoretree_panel
)
gain_ovr <- aggte(gain_attgt, type = "simple")
summary(gain_ovr)
gain_es <- aggte(gain_attgt, type = "dynamic")

schooltree_ovr_results <- data.frame("outcome" = "gain", "ATT" = gain_ovr$overall.att, "se" = gain_ovr$overall.se)%>%
  rbind(schooltree_ovr_results)
schooltree_es_results <- data.frame("outcome" = "gain", "ATT" = gain_es$att.egt, "e" = gain_es$egt, "se" = gain_es$se.egt)%>%
  rbind(schooltree_es_results)

library(rio)
#export(schooltree_ovr_results, "eab_tree_results_school3km.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### IVREG
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iv_panel <- full_panel %>%
  group_by(my_id)%>%
  arrange(year, .by_group = T)%>%
  #filter(year >= 2003 & year <= 2014)%>%
  mutate(cumloss = cumsum(acres_loss),
         cumgain = cumsum(acres_gain),
         cumnet = cumsum(acres_net_gain),
         treated = ifelse(year > first_detected & year > 0, 1, 0)
  )
library(fixest)

variable_names <- c("ISAT")
treatment_names <- c("cumnet", "cumloss", "cumgain")


iv_results <- data.frame()
for(i in variable_names){
  
  for(k in treatment_names){
    
    this_data <- iv_panel 
    
    
    x <- as.formula(paste(i,  '~ 1 | my_id + year |', k, "~ treated"))

    print(x)
    
    iv_est <- feols(x, this_data)
    coeff <- iv_est$coefficients
    se <- iv_est$se
    fstat <- fitstat(iv_est, "ivf")[[1]]$stat
    
 
    iv_results <- data.frame("outcome" = i,
                         "treatment" = k,
                         "coeff" = coeff,
                         "se" = se,
                         "Fstat" = fstat)%>%
      rbind(iv_results)
    
    

  }


}

library(rio)
export(iv_results, "iv_results_3km.rds")

test <- iv_results %>%
  mutate(significant.95 = ifelse(abs(coeff) >= 1.96*se, 1, 0),
         significant.9 = ifelse(abs(coeff) >= 1.645*se, 1, 0))


fs_results <- data.frame()
for(k in treatment_names){
  
  this_data <- iv_panel 
  
  fs <- as.formula(paste(k, "~ treated | my_id + year"))
  fs_est <- feols(fs, this_data)
  
  coeff <- fs_est$coefficients
  se <- fs_est$se
  fs_results <- data.frame("treatment" = k,
                         "coeff" = coeff,
                         "se" = se)%>%
  rbind(fs_results)
  
}

export(fs_results, "fs_results_3km.rds")
