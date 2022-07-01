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

counties_chicagoreg <- c("Cook", "DuPage", "Kane", "Kendall", "Lake", "McHenry", "Will")

illinois.shp <- read_sf("administrative/tl_2019_us_county/tl_2019_us_county.shp")%>%
  st_transform(crs(raster("tree_data/tree_loss_year.tif")))%>%
  filter(STATEFP == 17 & NAME %in% counties_chicagoreg)

tree_loss <- raster("tree_data/tree_loss_year.tif")%>%
  crop(illinois.shp)
tree_gain <- raster("tree_data/tree_gain_year.tif")%>%
  crop(illinois.shp)

roi <- illinois.shp %>%
  st_crop(tree_loss)

eab_infestations <- read_sf("eeb_infestations/eeb_address_geocode.shp")%>%
  dplyr::select(Date, City, Match_addr, City_1, Address)%>%
  separate(Date, c("Year", "Month", NA))%>%
  st_transform(crs(roi))%>%
  st_crop(roi)

reportcard_loc <- read_sf("schools/reportcard/cleaned/reportcard_loc.shp")%>%
  st_transform(crs(roi))%>%
  st_crop(roi)%>%
  select(RCDS, FacilityNa)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Extract tree loss and gain within area of school
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buffer_size <- 5

school_buffer <- reportcard_loc %>%
  st_buffer(buffer_size*1000)

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
  
}

school_treegainloss <- school_buffer

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### determine year of first EAB exposure
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

school_exposure <- school_treegainloss %>%
  st_join(eab_infestations)%>%
  group_by(RCDS)%>%
  mutate_at(vars(Year), as.numeric)%>%
  mutate(first_detected = min(Year, na.rm = T),
         first_exposed = ifelse(min(as.numeric(Month)) > 5, first_detected + 1, first_detected))%>%
  slice_head()%>%
  mutate_at(vars(first_exposed, first_detected), ~replace(., is.na(.), 0))

school_exposure$geometry <- NULL


eab_panel <- school_exposure %>%
  pivot_longer(cols = paste0("gain_",min_year):paste0("loss_",max_year),
               names_to = "type_year", 
               values_to = "total_change")%>%
  separate(type_year, into = c("change_type", "year"), sep = "_")%>%
  mutate_at(vars(year, first_exposed, first_detected), as.numeric)%>%
  pivot_wider(names_from = "change_type", values_from = "total_change")%>%
  group_by(year, RCDS)%>%
  slice_head()%>%
  mutate(net_gain = gain - loss,
         acres_gain = gain * 0.222395,
         acres_loss = loss * 0.222395,
         acres_net_gain = net_gain * 0.222395,
         treated_tree = ifelse(year >= first_detected & first_detected > 0, 1, 0),
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

panel <- panel_full %>%
  group_by(RCDS)%>%
  mutate(ID = cur_group_id())%>%
  mutate_at(vars(ID, k), as.numeric)


cov_names <- paste0("cov_", seq(from = 1, to = length(control_vars), by = 1))


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

tree_vars <- c("acres_gain", "acres_loss", "acres_net_gain")

ovr_results <- data.frame()
es_results <- data.frame()


for(k in tree_vars){
  print(k)
  
  attgt <- att_gt(yname = k,
                  tname = "year",
                  idname = "ID",
                  gname = "first_exposed",
                  control_group = "notyettreated",
                  xformla = as.formula(paste("~ ", paste(cov_names, collapse = " + "))),
                  data = panel
  )
  
  ovr <- aggte(attgt, type = "simple", na.rm = T)
  
  ovr_results <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se, "buffer" = i)%>%
    rbind(ovr_results)
  
  es <- aggte(attgt, type = "dynamic", na.rm = T)
  
  es_results <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "buffer" = i)%>%
    rbind(es_results)
  
}

ovr_results <- ovr_results %>%
  mutate(crit.val = ATT/se)

tree_gain_plot <- ggplot(es_results %>% filter(outcome == "acres_gain", 
                                               between(e, -8, 7)),
                         aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
tree_gain_plot

tree_netgain_plot <- ggplot(es_results %>% filter(outcome == "acres_net_gain", 
                                                  between(e, -8, 7)),
                            aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
tree_netgain_plot

tree_loss_plot <- ggplot(es_results %>% filter(outcome == "acres_loss", 
                                               between(e, -8, 7)),
                         aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
tree_loss_plot

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### heterogeneity in tree cover impacts analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(fixest)

tree_twfe <- feols(acres_net_gain ~ treated_tree | year + ID, data = panel)
summary(tree_twfe)

lowincome_twfe <- feols(acres_net_gain ~ treated_tree * as.numeric(`low-income school pct`)| year + ID, data = panel)
summary(lowincome_twfe)

lowincome_twfe <- feols(as.numeric(ISAT_composite) ~ treated_test * as.numeric(`low-income school pct`)| year + ID, data = panel)
summary(lowincome_twfe)

lowincome_twfe <- feols(as.numeric(`all_attendance rate school pct`) ~ treated_test * as.numeric(`low-income school pct`)| year + ID, data = panel)
summary(lowincome_twfe)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### DID Report card analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reportcard_outcomevars <- c("all_attendance rate school pct",    
               # "white_attendance rate school pct",  "black_attendance rate school pct", 
               "low income_attendance rate school pct",         
               "dropout rate  school pct", "all_tests",                  
               "ISAT_composite" ) 

ovr_results2 <- data.frame()
es_results2 <- data.frame()
  
  
for(k in reportcard_outcomevars){
  print(k)
  panel <- panel_full %>%
    group_by(RCDS)%>%
    mutate(ID = cur_group_id())%>%
    mutate_at(vars(ID, k), as.numeric)
    
    attgt <- att_gt(yname = k,
                    tname = "year",
                    idname = "ID",
                    gname = "first_exposed",
                    control_group = "notyettreated",
                    xformla = as.formula(paste("~ ", paste(cov_names, collapse = " + "))),
                    data = panel
    )
    
    ovr <- aggte(attgt, type = "simple", na.rm = T)
    
    ovr_results2 <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se, "buffer" = i)%>%
      rbind(ovr_results2)
    
    es <- aggte(attgt, type = "dynamic", na.rm = T)
    
    es_results2 <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "buffer" = i)%>%
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



