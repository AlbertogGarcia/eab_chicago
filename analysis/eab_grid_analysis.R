library(sf)
library(raster)
library(readxl)
library(tidyverse)
setwd("C:/Users/agarcia/Dropbox/eab_chicago_data")

library(ggmap)
AIzaSyAXYhVQFTZrMitEiOZl1VApTNBgEAACT7w
illinois_map <- get_map("Illinois", zoom = 7,maptype = "terrain", 
                              crop = FALSE)
ggmap(illinois_map)

illinois.shp <- read_sf("administrative/IL_State/IL_BNDY_State_Py.shp")%>%
  st_transform(crs(raster("tree_data/tree_loss_year.tif")))
tree_loss <- raster("tree_data/tree_loss_year.tif")%>%
  crop(illinois.shp)
tree_gain <- raster("tree_data/tree_gain_year.tif")%>%
  crop(illinois.shp)
impervious_increase <- raster("tree_data/IS_change_year.tif")%>%
  crop(illinois.shp)


eab_infestations <- read_sf("eeb_infestations/eeb_address_geocode.shp")%>%
  dplyr::select(Date, City, Match_addr, City_1, Address)%>%
  separate(Date, c("Year", NA, NA))%>%
  st_transform(crs(tree_loss))

mytheme <- theme(text = element_text(family = 'Avenir')
                 ,panel.grid.major = element_line(color = '#cccccc' 
                                                  ,linetype = 'dashed'
                                                  ,size = .3
                 )
                 ,panel.background = element_rect(fill = 'grey95')
                 # ,plot.title = element_text(size = 32)
                 # ,plot.subtitle = element_text(size = 14)
                 # ,axis.title = element_blank()
                 # ,axis.text = element_text(size = 10)
)

plot <- ggplot()+
  geom_sf(data = illinois.shp, fill = "papayawhip")+
  geom_sf(data = eab_infestations, aes(color = Year),size = 1.5)+
  labs(title = "  Illinois confirmed EAB infestations",
       fill = "Infestation year") + 
  theme_void()
plot



silvis_bound <- st_bbox(tree_loss)
box <- st_make_grid(silvis_bound, n = c(1,1))

eab_infestations_box <- eab_infestations %>%
  st_crop(box)

plot <- ggplot()+
  geom_sf(data = eab_infestations_box, aes(color = Year),size = 3)+
  geom_sf(data = box, fill = NA)+
  labs(title = "  Confirmed EAB infestation locations within tree cover data extent",
       fill = "Infestation year") + 
  theme_void()
plot


# grid_spacing = 3000
grid_spacing = 5000


# test <- st_make_grid(silvis_bound, n = c(1,1))
# st_write(test, "boundbox_treeloss.shp")
# plot(test)

gridded_infestations <- st_make_grid(silvis_bound, square = T, cellsize = c(grid_spacing, grid_spacing), crs = st_crs(eab_infestations)) %>% # the grid, covering bounding box
  st_sf() %>%# not really required, but makes the grid nicer to work with later
  mutate(grid = row_number())%>%
  st_join(eab_infestations)%>%
  group_by(grid)%>%
  mutate(first_detected = min(Year, na.rm = T))%>%
  slice_head()%>%
  dplyr::select(-c(Year))%>%
  ungroup()

plot <- ggplot()+
  geom_sf(data = gridded_infestations, aes(fill = first_detected))+
  labs(title = "  Year of first treatment by grid cell",
       fill = "First detection year") + 
  theme_void()
plot

#st_write(gridded_infestations, "gridded_infestations.shp")

library(ggplot2)
ggplot(eab_infestations %>% st_crop(gridded_infestations), aes(color = Year))+
  geom_sf(size = 1.5)
ggplot(gridded_infestations , aes(fill = first_detected))+
  geom_sf(size = 1.5)



eeb_loss_data <- gridded_infestations %>%
  st_crop(tree_loss)%>%
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
  mutate_at(vars(year, first_detected, grid), as.numeric)%>%
  pivot_wider(names_from = "change_type", values_from = "total_change")%>%
  group_by(year, grid)%>%
  slice_head()%>%
  mutate(net_gain = gain - loss,
         acres_gain = gain * 0.222395,
         acres_loss = loss * 0.222395,
         acres_net_gain = net_gain * 0.222395,
         acres_IS = impervious * 0.222395,
         treated = ifelse(year > first_detected & year > 0, 1, 0))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Tree cover loss
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


library(did)
set.seed(0930)
loss_attgt <- att_gt(yname = "acres_loss",
                tname = "year",
                idname = "grid",
                gname = "first_detected",
                control_group = "notyettreated",
                data = eeb_panel
)
loss_ovr <- aggte(loss_attgt, type = "simple")
summary(loss_ovr)
loss_es <- aggte(loss_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "loss", "ATT" = loss_ovr$overall.att, "se" = loss_ovr$overall.se)
es_results <- data.frame("outcome" = "loss", "ATT" = loss_es$att.egt, "e" = loss_es$egt, "se" = loss_es$se.egt)

library(fixest)
twfe_loss <- feols(acres_loss ~ treated | year + grid, data = eeb_panel)
summary(twfe_loss)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###  Tree cover gain
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gain_attgt <- att_gt(yname = "acres_gain",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = eeb_panel
)
summary(gain_attgt)

gain_ovr <- aggte(gain_attgt, type = "simple")
summary(gain_ovr)
gain_es <- aggte(gain_attgt, type = "dynamic")
twfe_cum <- feols(acres_gain ~ treated | year + grid, data = eeb_panel)
summary(twfe_cum)

ovr_results <- data.frame("outcome" = "gain", "ATT" = gain_ovr$overall.att, "se" = gain_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "gain", "ATT" = gain_es$att.egt, "e" = gain_es$egt, "se" = gain_es$se.egt)%>%
  rbind(es_results)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
###  tree cover change
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

net_attgt <- att_gt(yname = "acres_net_gain",
                    tname = "year",
                    idname = "grid",
                    gname = "first_detected",
                    control_group = "notyettreated",
                    data = eeb_panel
)
summary(net_attgt)

net_ovr <- aggte(net_attgt, type = "simple")
summary(net_ovr)
net_es <- aggte(net_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "net change", "ATT" = net_ovr$overall.att, "se" = net_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "net change", "ATT" = net_es$att.egt, "e" = net_es$egt, "se" = net_es$se.egt)%>%
  rbind(es_results)

twfe_net <- feols(acres_net_gain ~ treated | year + grid, data = eeb_panel)
summary(twfe_net)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Impervious increase
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

impervious_attgt <- att_gt(yname = "acres_IS",
                           tname = "year",
                           idname = "grid",
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

setwd("C:/Users/agarcia/Documents/eab_chicago/analysis")
library(rio)
export(ovr_results, "eab_tree_results_grid3km.rds")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Event study plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520")

loss_plot <- ggplot(es_results %>% filter(outcome == "loss" & e >= -10), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=lowerci,ymax=upperci),alpha=0.2, fill = palette$red)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+
  ggtitle("Tree cover loss (acres/year)")
loss_plot

netcover_plot <- ggplot(es_results %>% filter(outcome == "net change" & e >= -10), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=lowerci,ymax=upperci),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+
  ggtitle("Net tree cover change (acres/year)")
netcover_plot

gain_plot <- ggplot(es_results %>% filter(outcome == "gain" & e >= -10), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=lowerci,ymax=upperci),alpha=0.25, fill = palette$green)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+
  ggtitle("Tree cover gain (acres/year)")
gain_plot


