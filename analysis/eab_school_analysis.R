library(sf)
library(raster)
library(readxl)
library(tidyverse)
setwd("C:/Users/agarcia/Dropbox/chicago_eab")

illinois.shp <- read_sf("IL_State/IL_BNDY_State_Ln.shp")%>%
  st_transform(crs(raster("tree_data/tree_loss_year.tif")))

tree_loss <- raster("tree_data/tree_loss_year.tif")%>%
  crop(illinois.shp)
tree_gain <- raster("tree_data/tree_gain_year.tif")%>%
  crop(illinois.shp)
impervious_increase <- raster("tree_data/IS_change_year.tif")%>%
  crop(illinois.shp)

school_districts.shp <- read_sf("SCHOOLDISTRICT_SY1314_TL15/schooldistrict_sy1314_tl15.shp")%>%
  st_transform(crs(tree_loss))%>%
  filter(STATEFP == 17)%>%
  st_crop(extent(tree_loss))



eab_infestations <- read_sf("eeb_infestations/eeb_address_geocode.shp")%>%
  dplyr::select(Date, City, Match_addr, City_1, Address)%>%
  separate(Date, c("Year", NA, NA))%>%
  st_transform(crs(tree_loss))



district_infestations <- school_districts.shp %>%
  st_join(eab_infestations)%>%
  group_by(GEOID)%>%
  mutate(first_detected = min(Year, na.rm = T))%>%
  slice_head()%>%
  dplyr::select(-c(Year))%>%
  ungroup()


plot(district_infestations %>% dplyr::select(first_detected))


eeb_loss_data <- district_infestations %>%
  mutate_at(vars(first_detected), ~replace(., is.na(.), 0))

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

eeb_loss_data$geometry = NULL



eeb_panel <- eeb_loss_data %>%
  pivot_longer(cols = paste0("gain_",min_year):paste0("impervious_",max_year),
               names_to = "type_year", 
               values_to = "total_change")%>%
  separate(type_year, into = c("change_type", "year"), sep = "_")%>%
  mutate_at(vars(year, first_detected, GEOID), as.numeric)%>%
  pivot_wider(names_from = "change_type", values_from = "total_change")%>%
  group_by(year, GEOID)%>%
  mutate(net_gain = gain - loss,
         treated = ifelse(year > first_detected & first_detected > 0, 1, 0))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Estimation: Tree cover 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Tree cover loss
library(did)
set.seed(0930)
loss_attgt <- att_gt(yname = "loss",
                     tname = "year",
                     idname = "GEOID",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = eeb_panel
)
summary(loss_attgt)
loss_ovr <- aggte(loss_attgt, type = "simple")
summary(loss_ovr)
loss_es <- aggte(loss_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "loss", "ATT" = loss_ovr$overall.att, "se" = loss_ovr$overall.se)
es_results <- data.frame("outcome" = "loss", "ATT" = loss_es$att.egt, "e" = loss_es$egt, "se" = loss_es$se.egt)

library(fixest)
twfe_loss <- feols(loss ~ treated | year + GEOID, data = eeb_panel)
summary(twfe_loss)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Net tree cover change
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


net_attgt <- att_gt(yname = "net_gain",
                    tname = "year",
                    idname = "GEOID",
                    gname = "first_detected",
                    control_group = "notyettreated",
                    data = eeb_panel
)
summary(net_attgt)

net_ovr <- aggte(net_attgt, type = "simple")
summary(net_ovr)
net_es <- aggte(net_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "net gain", "ATT" = net_ovr$overall.att, "se" = net_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "net gain", "ATT" = net_es$att.egt, "e" = net_es$egt, "se" = net_es$se.egt)%>%
  rbind(es_results)

twfe_net <- feols(net_gain ~ treated | year + GEOID, data = eeb_panel)
summary(twfe_net)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Impervious increase
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

impervious_attgt <- att_gt(yname = "impervious",
                           tname = "year",
                           idname = "GEOID",
                           gname = "first_detected",
                           control_group = "notyettreated",
                           data = eeb_panel
)

impervious_ovr <- aggte(impervious_attgt, type = "simple")
summary(impervious_ovr)
impervious_es <- aggte(impervious_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "impervious", "ATT" = impervious_ovr$overall.att, "se" = impervious_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "impervious", "ATT" = impervious_es$att.egt, "e" = impervious_es$egt, "se" = impervious_es$se.egt)%>%
  rbind(es_results)

es_results <- es_results %>%
  mutate(upperci = ATT + 1.96*se,
         lowerci = ATT - 1.96*se)


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

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Read in test score data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stopwords = c("SD", "school district", "cusd", "sd", "ud", "high")

removeWords <- function(str) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}

district_ids <- read_excel("test_scores/school_07.xls")%>%
  rename(Dist_num = 3,
         Dist_name = 4)%>%
  select(Dist_num, Dist_name)%>%
    group_by(Dist_num)%>%
    slice_head()%>%
  mutate(Dist_name = removeWords(tolower(Dist_name)))

library(fuzzyjoin)

school_districts <- school_districts.shp %>%
  select(GEOID, NAME)%>%
  mutate(Dist_name = tolower(NAME))%>%
  stringdist_inner_join(district_ids, by = c("Dist_name"), max_dist = 4)




















files <- list.files(path = "test_scores", recursive = TRUE,
                    pattern = ".xls",
                    full.names = TRUE
)

temp = sapply(files, read_excel, simplify = FALSE
              #,  colClasses="character"
)
library(data.table)
test_scores <- rbindlist(temp,
                         use.names = TRUE, idcol = "filename", 
                         fill = TRUE)%>%
  rename(Dist = 3)%>%
  separate(filename, into = c(NA, NA, NA, "Year", NA))%>%
  mutate(GEOID = as.character(paste0("17", Dist, sep = "")),
         year = as.numeric(paste0("20", Year, sep = ""))
  )%>%
  select("District Name/ School Name", year, GEOID, Read_3, Math_3, ACT)%>%
  rename(Dist_name = 1)

test_panel <- eeb_panel %>%
  mutate(GEOID = as.character(GEOID))%>%
  inner_join(test_scores, by = c("GEOID", "year"))%>%
  select(year, GEOID, NAME, Dist_name)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Combine test scores and tree cover into panel
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
