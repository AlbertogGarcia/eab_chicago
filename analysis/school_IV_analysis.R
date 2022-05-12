library(sf)
library(raster)
library(readxl)
library(tidyverse)
library(here)

setwd("C:/Users/garci/Dropbox/eab_chicago_data")

illinois.shp <- read_sf("administrative/IL_State/IL_BNDY_State_Ln.shp")%>%
  st_transform(crs(raster("tree_data/tree_loss_year.tif")))
tree_loss <- raster("tree_data/tree_loss_year.tif")%>%
  crop(illinois.shp)
tree_gain <- raster("tree_data/tree_gain_year.tif")%>%
  crop(illinois.shp)
impervious_increase <- raster("tree_data/IS_change_year.tif")%>%
  crop(illinois.shp)

school_test_scores <- readRDS("schools/school_test_scores.rds")


buffer_size <- 5000

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
  
  this_impervious_increase <- impervious_increase
  this_impervious_increase[this_impervious_increase[] != i ] = 0
  this_impervious_increase[this_impervious_increase[] == i ] = 1
  
  new <- exact_extract(this_impervious_increase, school_buffer, 'sum')
  school_buffer[ , ncol(school_buffer) + 1] <- new      # Append new column
  colnames(school_buffer)[ncol(school_buffer)] <- paste0("impervious_", i)
  
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
  mutate_at(vars(first_detected), ~replace(., is.na(.), 0))%>%
  filter(first_detected == min(first_detected))%>%
  slice_head()%>%
  ungroup()%>%
  select(-Year)

school_infestation$geometry <- NULL

schooltree_panel <- school_infestation %>%
  pivot_longer(cols = paste0("gain_",min_year):paste0("impervious_",max_year),
               names_to = "type_year", 
               values_to = "total_change")%>%
  separate(type_year, into = c("change_type", "year"), sep = "_")%>%
  mutate_at(vars(year, first_detected, my_id), as.numeric)%>%
  pivot_wider(names_from = "change_type", values_from = "total_change")%>%
  group_by(year, my_id)%>%
  slice_head()%>%
  ungroup()%>%
  group_by(my_id)%>%
  arrange(year, .by_group = T)%>%
  mutate(net_gain = gain - loss,
         acres_gain = gain * 0.222395,
         acres_loss = loss * 0.222395,
         acres_net_gain = net_gain * 0.222395,
         acres_IS = impervious * 0.222395,
         cumloss = cumsum(acres_loss),
         cumgain = cumsum(acres_gain),
         cumnet = cumsum(acres_net_gain),
         treated = ifelse(year >= first_detected & year > 0, 1, 0)
         )

school_test_scores$geometry <- NULL

full_panel <- schooltree_panel %>%
  left_join(school_test_scores, by = c("my_id", "year"))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### IVREG
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

iv_panel <- full_panel %>%
  group_by(my_id)%>%
  arrange(year, .by_group = T)%>%
  filter(year >= 2003 & year <= 2014)%>%
  mutate(cumloss = cumsum(acres_loss),
         cumgain = cumsum(acres_gain),
         cumnet = cumsum(acres_net_gain)
  )

library(estimatr) # outcome ~ treatment | instrument
library(AER)
variable_names <- c("ISAT")#, "Math_3", "Math_5", "Math_8", "Math_11", "ACT", "Read_3", "Read_5", "Read_8", "Read_11")
treatment_names <- c("cumnet", "cumloss", "cumgain", "acres_loss", "acres_gain", "acres_net_gain")

allModelsList <- lapply(paste(variable_names, "~", treatment_names, "+ as.factor(year) + as.factor(my_id) | treated + as.factor(year) + as.factor(my_id)"), as.formula)

iv_results <- data.frame()

for(i in variable_names){
  
  for(k in treatment_names){
    
    this_data <- iv_panel 
    
    first_stage_formula <- as.formula(paste(k, "~ treated + as.factor(year) + as.factor(my_id)"))
    fs_sum <- summary(lm(first_stage_formula, data = this_data))
    Fstat <- fs_sum$fstatistic[1]
    x <- as.formula(paste(i,  '~', k, "+ as.factor(year) + as.factor(my_id) | treated + as.factor(year) + as.factor(my_id)"))

    print(x)
    
    iv2sls <- summary(ivreg(x,
                        data = full_panel %>% drop_na(i)
                        )
    )$coefficients[2,]
    
 
    iv_results <- data.frame("outcome" = i,
                         "treatment" = k,
                         "coeff" = iv2sls[1],
                         "se" = iv2sls[2],
                         "Fstat" = Fstat[1])%>%
      rbind(iv_results)

  }


}

library(rio)
export(iv_results, "iv_results_5km.rds")

test <- iv_results %>%
  mutate(significant.95 = ifelse(abs(coeff) >= 1.96*se, 1, 0),
         significant.9 = ifelse(abs(coeff) >= 1.645*se, 1, 0))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#######
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(did)
set.seed(0930)
loss_attgt <- att_gt(yname = "acres_gain",
                     tname = "year",
                     idname = "my_id",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = iv_panel
)
loss_ovr <- aggte(loss_attgt, type = "simple")
summary(loss_ovr)
loss_es <- aggte(loss_attgt, type = "dynamic")
ggdid(loss_es)

ovr_results <- data.frame("outcome" = "loss", "ATT" = loss_ovr$overall.att, "se" = loss_ovr$overall.se)
es_results <- data.frame("outcome" = "loss", "ATT" = loss_es$att.egt, "e" = loss_es$egt, "se" = loss_es$se.egt)

library(fixest)
twfe_loss <- feols(acres_loss ~ treated | year + grid, data = eeb_panel)
summary(twfe_loss)

